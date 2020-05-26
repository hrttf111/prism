module Prism.Run where

import Control.Monad.Trans (liftIO)

import Data.Array ((!))

import Numeric (showHex)

import Prism.Cpu
import Prism.Decoder

-------------------------------------------------------------------------------

runPeripheralsR :: PrismM ()
runPeripheralsR = do
    needUpdate <- needUpdateP
    if needUpdate then
        runP
    else
        decCyclesP

runInstructionM :: PrismDecoder -> MemOffset -> PrismM ()
runInstructionM dec offset = do
    instr <- peekInstrBytesM offset
    let (b1, _, _, _, _, _) = instr
        func = instrFunc $ (decInstr dec) ! b1
    liftIO $ putStrLn (showHex b1 "")
    --execTF <$> func instr
    func instr

runInstructionM1 :: PrismDecoder -> MemOffset -> PrismM ()
runInstructionM1 dec offset = do
    instr <- peekInstrBytesM offset
    let (b1, _, _, _, _, _) = instr
        func = instrFunc $ (decInstr dec) ! b1
    liftIO $ putStrLn (showHex b1 "")
    func instr

runCpu :: PrismDecoder -> MemOffset -> (PrismM ()) -> PrismM ()
runCpu dec offset cont = do
    intActive <- interruptActive
    if intActive then
        processInterrupts
            >> runPeripheralsR
            >> cont
    else
        runInstructionM dec offset
            >> runPeripheralsR
            >> cont

-------------------------------------------------------------------------------

decodeExecOne :: PrismDecoder -> PrismM ()
decodeExecOne dec =
    instrAddress >>= runInstructionM1 dec

decodeMemIp :: PrismDecoder -> Int -> PrismM ()
decodeMemIp dec len = do
    offset <- instrAddress
    if (fromIntegral offset) >= len then return ()
    else
        runCpu dec offset (decodeMemIp dec len)

{-
decodeHaltCpu :: PrismDecoder -> PrismComm -> PrismM ()
decodeHaltCpu dec comm = do
    offset <- instrAddress
    m <- processPrismCommand comm ctx offset
    case m of
        Just (comm_, ctx_) ->
            decodeHaltCpu dec comm_ ctx_
        Nothing ->
            runCpu dec offset ctx (decodeHaltCpu dec comm)
            -}

-------------------------------------------------------------------------------
