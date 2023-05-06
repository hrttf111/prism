{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Prism.Cpu.Trans where

import Control.Monad.Trans
import Control.Monad.State.Strict

import Data.Bits (shiftL)

import Prism.Cpu.Types
import Prism.Cpu.Monad
import Prism.Cpu.Flags
import Prism.Cpu.Memory
import Prism.Cpu.Registers
import Prism.Cpu.Ports
import Prism.Cpu.Interrupt
import Prism.Cpu.Peripherals

-------------------------------------------------------------------------------

instance CpuMonad CpuTrans where
    cpuHalt = modify (\s -> s { ctxStop = True } )
    cpuOverrideSegment regSeg = modify (\s -> s { ctxReplaceSeg = regSeg } )
    cpuUpdateIP val = ((val +) <$> readOp ip) >>= writeOp ip
    cpuInstrAddress = do
        valCs <- fromIntegral <$> readOp cs
        valIp <- fromIntegral <$> readOp ip
        return $ (shiftL valCs 4) + valIp
    cpuNextInstrByte = do
        ctx <- get
        peekFirstByte (ctxMem ctx) =<< cpuInstrAddress
    cpuTick = do
        ctx <- get
        let newCycles = (ctxCycles ctx) + 1
            newCyclesP = (ctxCyclesP ctx) - 1
            updatePeripherals = (newCyclesP <= 0)
            interruptActive = intInterruptUp . ctxInterrupts $ ctx
        put $ ctx { ctxCycles = newCycles, ctxCyclesP = newCyclesP }
        return (updatePeripherals, interruptActive)
    cpuRunDirect command = do
        ctxIO <$> get >>= f
        where
            f (IOCtx s _ _) = runPeripheralsDirect s command

-------------------------------------------------------------------------------
