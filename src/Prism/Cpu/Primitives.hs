{-# LANGUAGE FlexibleContexts #-}

module Prism.Cpu.Primitives where

import Prism.Cpu.Types
import Prism.Cpu.Registers
import Prism.Cpu.Memory

-------------------------------------------------------------------------------

push16 :: (CpuMonad m) => Uint16 -> m ()
push16 val = do
    valSp <- readOp sp
    writeOp sp (valSp - 2)
    writeOp (MemSeg16 MemSp) val

pop16 :: (CpuMonad m) => m Uint16
pop16 = do
    val <- readOp (MemSeg16 MemSp)
    valSp <- readOp sp
    writeOp sp (valSp + 2)
    return val 

pushP :: (CpuMonad m, Operand a m Uint16) => a -> m ()
pushP op =
    readOp op >>= push16

popP :: (CpuMonad m, Operand a m Uint16) => a -> m ()
popP op =
    pop16 >>= writeOp op

-------------------------------------------------------------------------------
