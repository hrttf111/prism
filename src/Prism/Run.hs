module Prism.Run where

import Control.Monad.Trans (liftIO)
import Control.Monad.State.Strict

import Data.Array ((!))

import Numeric (showHex)

import Prism.Cpu
import Prism.Decoder
import Prism.Command
import qualified Prism.Log as Log

-------------------------------------------------------------------------------

runPeripherals' :: Bool -> PrismM ()
runPeripherals' updatePeripherals =
    if updatePeripherals then
        f =<< ctxIO <$> get
        else
            return ()
    where
        f (IOCtx a _ _) = runPeripheralsM a $ runPeripherals

{-# INLINE runPeripherals' #-}

runCpuInstruction :: PrismDecoder -> MemOffset -> PrismM ()
runCpuInstruction dec offset = do
    instr <- peekInstrBytesM offset
    let (b1, _, _, _, _, _) = instr
        func = instrFunc $ (decInstr dec) ! b1
    Log.cpuLogT Trace Log.PrismRun $ "0x" ++ (showHex offset "") ++ ": " ++ (showHex b1 "")
    func instr

{-# INLINE runCpuInstruction #-}

runPrism :: PrismDecoder -> MemOffset -> PrismM Bool
runPrism dec offset = do
    (updatePeripherals, interruptActive) <- cpuTick
    if interruptActive then
        processInterrupts
        >> runPeripherals' updatePeripherals
        >> return False
    else
        runCpuInstruction dec offset
        >> runPeripherals' updatePeripherals
        >> ctxStop <$> get

{-# INLINE runPrism #-}

-------------------------------------------------------------------------------

decodeExecOne :: PrismDecoder -> PrismM ()
decodeExecOne dec =
    cpuInstrAddress >>= runCpuInstruction dec

decodeMemIp :: PrismDecoder -> Int -> PrismM ()
decodeMemIp dec len = do
    offset <- cpuInstrAddress
    if (fromIntegral offset) >= len then return ()
    else do
        runPrism dec offset
        decodeMemIp dec len

decodeHaltCpu :: PrismDecoder -> PrismComm -> PrismM ()
decodeHaltCpu dec comm = do
    offset <- cpuInstrAddress
    comm_ <- processPrismCommand comm offset
    if (commWaitResponse comm_) then
        decodeHaltCpu dec comm_
        else do
            stop <- runPrism dec offset
            if stop then return ()
                else
                    decodeHaltCpu dec comm_

-------------------------------------------------------------------------------
