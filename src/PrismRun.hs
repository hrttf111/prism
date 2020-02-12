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

decodeOffset :: Ctx -> PrismCtx IO MemOffset
decodeOffset ctx = 
    readRegIP memReg >>= getInstrAddress memReg cs
    where
        memReg = ctxReg ctx

runPeripheralsM :: Ctx -> PrismCtx IO Ctx
runPeripheralsM ctx =
    if (ctxCycles ctx) == 0 then
        processPeripherals ctx
    else do
        doUpdate <- liftIO $ needUpdate (ctxIO ctx)
        if doUpdate then
            updatePeripherals ctx
        else
            decCycles ctx

runInstructionM :: PrismDecoder -> MemOffset -> Ctx -> PrismCtx IO Ctx
runInstructionM dec offset ctx = do
    instr <- peekInstrBytes (ctxMem ctx) offset
    let (b1, _, _, _, _, _) = instr
        func = instrFunc $ (decInstr dec) ! b1
    liftIO $ putStrLn (showHex b1 "")
    execTF <$> func instr ctx

runInstructionM1 :: PrismDecoder -> MemOffset -> Ctx -> PrismCtx IO Ctx
runInstructionM1 dec offset ctx = do
    instr <- peekInstrBytes (ctxMem ctx) offset
    let (b1, _, _, _, _, _) = instr
        func = instrFunc $ (decInstr dec) ! b1
    liftIO $ putStrLn (showHex b1 "")
    func instr ctx

runCpu :: PrismDecoder -> MemOffset -> Ctx -> (Ctx -> PrismCtx IO Ctx) -> PrismCtx IO Ctx
runCpu _ _ ctx _ | ctxStop ctx = return ctx
runCpu dec offset ctx cont =
    if interruptActive ctx then
        processInterrupts ctx
            >>= runPeripheralsM
            >>= cont
    else
        runInstructionM dec offset ctx
            >>= runPeripheralsM
            >>= cont

-------------------------------------------------------------------------------

decodeExecOne :: PrismDecoder -> Ctx -> PrismCtx IO Ctx 
decodeExecOne dec ctx = do
    offset <- decodeOffset ctx
    runInstructionM1 dec offset ctx


decodeMemIp :: PrismDecoder -> Int -> Ctx -> PrismCtx IO Ctx
decodeMemIp dec len ctx = do
    offset <- decodeOffset ctx
    if (fromIntegral offset) >= len then return ctx
    else
        runCpu dec offset ctx (decodeMemIp dec len)


decodeHaltCpu :: PrismDecoder -> PrismComm -> Ctx -> PrismCtx IO Ctx
decodeHaltCpu dec comm ctx = do
    offset <- decodeOffset ctx
    m <- processPrismCommand comm ctx offset
    case m of
        Just (comm_, ctx_) ->
            decodeHaltCpu dec comm_ ctx_
        Nothing ->
            runCpu dec offset ctx (decodeHaltCpu dec comm)

-------------------------------------------------------------------------------
