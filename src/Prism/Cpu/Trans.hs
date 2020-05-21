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

-------------------------------------------------------------------------------

instance CpuMonad CpuTrans where
    halt = modify (\s -> s { ctxStop = True } )
    incCycles = modify (\s -> s { ctxCycles = ((ctxCycles s) + 1) } )
    updateIP val = ((val +) <$> readOp ip) >>= writeOp ip
    instrAddress = do
        valCs <- fromIntegral <$> readOp cs
        valIp <- fromIntegral <$> readOp ip
        return $ (shiftL valCs 4) + valIp
    needStop = ctxStop <$> get
    overrideSegment regSeg = modify (\s -> s { ctxReplaceSeg = regSeg } )
    nextInstrByte = do
        ctx <- get
        peekFirstByte (ctxMem ctx) =<< instrAddress

-------------------------------------------------------------------------------
