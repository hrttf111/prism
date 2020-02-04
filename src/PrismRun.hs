module PrismRun where

import Numeric (showHex)

import Data.Maybe (fromJust)
import Data.Array (Array, array, accumArray, (!), bounds)
import Control.Monad.Trans (lift, liftIO, MonadIO)

import Foreign.Ptr
import Foreign.Storable (peekByteOff, pokeByteOff)

import Prism
import PrismCpu
import PrismInterrupt
import PrismCommand
import PrismPeripheral
import PrismDecoder

-------------------------------------------------------------------------------

decodeList :: PrismDecoder -> Ctx -> [InstrBytes] -> PrismCtx IO Ctx
decodeList _ ctx [] = return ctx
decodeList dec ctx (x:xs) = do
    ctx1 <- instr x ctx
    decodeList dec ctx1 xs
    where
        (b1, _, _, _, _, _) = x
        instr = instrFunc $ (decInstr dec) ! b1

decodeExecOne :: PrismDecoder -> Ctx -> PrismCtx IO Ctx 
decodeExecOne dec ctx = do
    offset <- getInstrAddress memReg cs =<< readRegIP memReg
    instr <- peekInstrBytes (ctxMem ctx) offset
    let (b1, _, _, _, _, _) = instr
        func = instrFunc $ (decInstr dec) ! b1
    liftIO $ putStrLn (showHex b1 "_One")
    func instr ctx
    where
        memReg = ctxReg ctx

decodeMemIp :: PrismDecoder -> Int -> Ctx -> PrismCtx IO Ctx
decodeMemIp dec len ctx = do
    offset <- getInstrAddress memReg cs =<< readRegIP memReg
    if (fromIntegral offset) >= len then return ctx
    else do
        if interruptActive ctx then processInterrupts ctx >>= decodeMemIp dec len
        else do
            instr <- peekInstrBytes (ctxMem ctx) offset
            let (b1, _, _, _, _, _) = instr
                func = instrFunc $ (decInstr dec) ! b1
            liftIO $ putStrLn (showHex b1 "")
            execTF <$> func instr ctx >>= decodeMemIp dec len
    where
        memReg = ctxReg ctx

decodeHalt :: PrismDecoder -> Ctx -> PrismCtx IO Ctx
decodeHalt dec ctx = do
    if ctxStop ctx then return ctx
    else do
        if interruptActive ctx then processInterrupts ctx >>= decodeHalt dec
        else do
            offset <- getInstrAddress memReg cs =<< readRegIP memReg
            instr <- peekInstrBytes (ctxMem ctx) offset
            let (b1, _, _, _, _, _) = instr
                func = instrFunc $ (decInstr dec) ! b1
            liftIO $ putStrLn (showHex b1 "")
            execTF <$> func instr ctx >>= decodeHalt dec 
    where
        memReg = ctxReg ctx

decodeHaltCpu :: PrismDecoder -> PrismComm -> Ctx -> PrismCtx IO Ctx
decodeHaltCpu dec comm ctx = do
    offset <- getInstrAddress memReg cs =<< readRegIP memReg
    m <- processPrismCommand comm ctx offset
    case m of
        Just (comm_, ctx_) -> decodeHaltCpu dec comm_ ctx_
        Nothing -> 
            if ctxStop ctx then return ctx
            else if (ctxCycles ctx) == 0 then
                    processPeripherals ctx >>= decodeHaltCpu dec comm
                else
                    if interruptActive ctx then
                        processInterrupts ctx >>= decodeHaltCpu dec comm
                    else do
                        instr <- peekInstrBytes (ctxMem ctx) offset
                        let (b1, _, _, _, _, _) = instr
                            func = instrFunc $ (decInstr dec) ! b1
                        liftIO $ putStrLn (showHex b1 "")
                        execTF <$> func instr ctx >>= decCycles >>= decodeHaltCpu dec comm
    where
        memReg = ctxReg ctx

-------------------------------------------------------------------------------
