{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Prism.Cpu.Primitives where

import Prism.Cpu.Types
import Prism.Cpu.Registers
import Prism.Cpu.Memory

-------------------------------------------------------------------------------

type MemRegM m = ( Monad m
                 , Operand Reg8 m Uint8
                 , Operand Reg16 m Uint16
                 , Operand RegSeg m Uint16
                 , Operand RegSpec m Uint16
                 , Operand MemSeg8 m Uint8
                 , Operand MemSeg16 m Uint16
                 , Operand MemSegExp8 m Uint8
                 , Operand MemSegExp16 m Uint16
                 , Operand MemPhy8 m Uint8
                 , Operand MemPhy16 m Uint16
                 , MemAddress MemSeg8 m Uint8
                 , MemAddress MemSeg16 m Uint16
                 , Operand Port8 m Uint8
                 , Operand Port16 m Uint16
                 )

-------------------------------------------------------------------------------

push16 :: (MemRegM m) => Uint16 -> m ()
push16 val = do
    valSp <- readOp sp
    writeOp sp (valSp - 2)
    writeOp (MemSeg16 MemSp) val

pop16 :: (MemRegM m) => m Uint16
pop16 = do
    val <- readOp (MemSeg16 MemSp)
    valSp <- readOp sp
    writeOp sp (valSp + 2)
    return val 

-------------------------------------------------------------------------------

pushP :: (MemRegM m, Operand a m Uint16) => a -> m ()
pushP op =
    readOp op >>= push16

popP :: (MemRegM m, Operand a m Uint16) => a -> m ()
popP op =
    pop16 >>= writeOp op

-------------------------------------------------------------------------------

pushV :: (MemRegM m) => Uint16 -> m ()
pushV = push16

popV :: (MemRegM m) => m Uint16
popV = pop16

-------------------------------------------------------------------------------

modifyFlag :: (CpuFlag a m) => (Bool -> Bool) -> a -> m ()
modifyFlag func flag =
    (func <$> getFlag flag) >>= setFlag flag

-------------------------------------------------------------------------------
