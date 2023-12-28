{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Prism.Instruction where

import Data.Word (Word8, Word16)

import Prism.Cpu
import qualified Prism.Log as Log

import qualified Prism.InstructionM as M

-------------------------------------------------------------------------------

haltFunc msg _ = do
    Log.cpuLogT Debug Log.CpuStrings msg
    cpuHalt

-------------------------------------------------------------------------------

type OperandFunc1 a v = M.OperandFunc1 a PrismM v
type OperandFunc2 a1 a2 v = M.OperandFunc2 a1 a2 PrismM v

--Plain function
--decoder is main user
type FuncImplicit = M.FuncImplicit PrismM

--Takes immediate values
--decoder is main user
type FuncImm1 i = M.FuncImm1 i PrismM
type FuncImm2 i = M.FuncImm2 i PrismM

--Takes and modifies operand
--executor is main user
type FuncO1M a = M.FuncO1M a PrismM
type FuncO2M a1 a2 = M.FuncO2M a1 a2 PrismM

--Takes immediate value + operand
--decoder is main user
type FuncOI1M a v = M.FuncOI1M a PrismM v

--Takes and modifies value+monad from operand
--executor is main user
type FuncV1M v = M.FuncV1M v PrismM
type FuncNV1M v = M.FuncNV1M v PrismM

--Takes and modifies value from operand
--executor is main user
type FuncV1 v = M.FuncV1 v
type FuncV2 v = M.FuncV2 v

--Takes and modifies value from operand
--executor is main user
type FuncVF1 v = M.FuncVF1 v
type FuncVF2 v = M.FuncVF2 v

-------------------------------------------------------------------------------

instrON1 :: (OperandFunc1 a v) => FuncNV1M v -> FuncO1M a
instrON1 = M.instrON1

{-# SPECIALISE instrON1 :: FuncNV1M Word8 -> FuncO1M Reg8 #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 -> FuncO1M Reg16 #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 -> FuncO1M RegSeg #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word8 -> FuncO1M MemSeg8 #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 -> FuncO1M MemSeg16 #-}

instrOM1 :: (OperandFunc1 a v) => FuncV1M v -> FuncO1M a
instrOM1 = M.instrOM1

{-# SPECIALISE instrOM1 :: FuncV1M Word8 -> FuncO1M Reg8 #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word16 -> FuncO1M Reg16 #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word16 -> FuncO1M RegSeg #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word8 -> FuncO1M MemSeg8 #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word16 -> FuncO1M MemSeg16 #-}

-------------------------------------------------------------------------------

instrO1 :: (OperandFunc1 a v) => FuncV1 v -> FuncO1M a
instrO1 = M.instrO1

{-# SPECIALISE instrO1 :: FuncV1 Word8 -> FuncO1M Reg8 #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M Reg16 #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M RegSeg #-}
{-# SPECIALISE instrO1 :: FuncV1 Word8 -> FuncO1M MemSeg8 #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M MemSeg16 #-}

instrOI1 :: (OperandFunc1 a v) => FuncV2 v -> FuncOI1M a v
instrOI1 = M.instrOI1

{-# SPECIALISE instrOI1 :: FuncV2 Word8 -> FuncOI1M Reg8 Word8 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M Reg16 Word16 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M RegSeg Word16 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word8 -> FuncOI1M MemSeg8 Word8 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M MemSeg16 Word16 #-}

instrOI1w :: (OperandFunc1 a v) => FuncV2 v -> FuncOI1M a v
instrOI1w = M.instrOI1w

{-# SPECIALISE instrOI1w :: FuncV2 Word8 -> FuncOI1M Reg8 Word8 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word16 -> FuncOI1M Reg16 Word16 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word16 -> FuncOI1M RegSeg Word16 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word8 -> FuncOI1M MemSeg8 Word8 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word16 -> FuncOI1M MemSeg16 Word16 #-}

instrO2 :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrO2 = M.instrO2

{-# SPECIALISE instrO2 :: FuncV2 Word8 -> FuncO2M Reg8 Reg8 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M Reg16 Reg16 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word8 -> FuncO2M MemSeg8 Reg8 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word8 -> FuncO2M Reg8 MemSeg8#-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M Reg16 MemSeg16 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M MemSeg16 Reg16 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M RegSeg Reg16 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M Reg16 RegSeg #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M RegSeg MemSeg16 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M MemSeg16 RegSeg #-}

instrO2w :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrO2w = M.instrO2w

{-# SPECIALISE instrO2w :: FuncV2 Word8 -> FuncO2M Reg8 Reg8 #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M Reg16 Reg16 #-}
{-# SPECIALISE instrO2w :: FuncV2 Word8 -> FuncO2M MemSeg8 Reg8 #-}
{-# SPECIALISE instrO2w :: FuncV2 Word8 -> FuncO2M Reg8 MemSeg8#-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M Reg16 MemSeg16 #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M MemSeg16 Reg16 #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M RegSeg Reg16 #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M Reg16 RegSeg #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M RegSeg MemSeg16 #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M MemSeg16 RegSeg #-}

-------------------------------------------------------------------------------

instrOF1 :: (OperandFunc1 a v) => FuncVF1 v -> FuncO1M a
instrOF1 = M.instrOF1

{-# SPECIALISE instrOF1 :: FuncVF1 Word8 -> FuncO1M Reg8 #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word16 -> FuncO1M Reg16 #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word16 -> FuncO1M RegSeg #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word8 -> FuncO1M MemSeg8 #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word16 -> FuncO1M MemSeg16 #-}

instrOFI1 :: (OperandFunc1 a v) => FuncVF2 v -> FuncOI1M a v
instrOFI1 = M.instrOFI1

{-# SPECIALISE instrOFI1 :: FuncVF2 Word8 -> FuncOI1M Reg8 Word8 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word16 -> FuncOI1M Reg16 Word16 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word16 -> FuncOI1M RegSeg Word16 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word8 -> FuncOI1M MemSeg8 Word8 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word16 -> FuncOI1M MemSeg16 Word16 #-}

instrOF2 :: (OperandFunc2 a1 a2 v) => FuncVF2 v -> FuncO2M a1 a2
instrOF2 = M.instrOF2

{-# SPECIALISE instrOF2 :: FuncVF2 Word8 -> FuncO2M Reg8 Reg8 #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M Reg16 Reg16 #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word8 -> FuncO2M MemSeg8 Reg8 #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word8 -> FuncO2M Reg8 MemSeg8 #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M Reg16 MemSeg16 #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M MemSeg16 Reg16 #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M RegSeg Reg16 #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M Reg16 RegSeg #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M RegSeg MemSeg16 #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M MemSeg16 RegSeg #-}

-------------------------------------------------------------------------------

instrOp1ToOp2 :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrOp1ToOp2 = instrO2

instrOp2ToOp1 :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrOp2ToOp1 func op1 op2 = instrO2 func op2 op1

instrRmToReg :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrRmToReg = instrOp1ToOp2

instrRegToRm :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrRegToRm = instrOp2ToOp1

-------------------------------------------------------------------------------

instrOp1ToOp2F :: (OperandFunc2 a1 a2 v) => FuncVF2 v -> FuncO2M a1 a2
instrOp1ToOp2F = M.instrOp1ToOp2F

instrOp2ToOp1F :: (OperandFunc2 a1 a2 v) => FuncVF2 v -> FuncO2M a1 a2
instrOp2ToOp1F = M.instrOp2ToOp1F

instrRmToRegF :: (OperandFunc2 a1 a2 v) => FuncVF2 v -> FuncO2M a1 a2
instrRmToRegF = M.instrRmToRegF

instrRegToRmF :: (OperandFunc2 a1 a2 v) => FuncVF2 v -> FuncO2M a1 a2
instrRegToRmF = M.instrRegToRmF

-------------------------------------------------------------------------------

instrOp1ToOp2w :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrOp1ToOp2w = instrO2w

instrOp2ToOp1w :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrOp2ToOp1w func op1 op2 = instrO2w func op2 op1

instrRmToRegw :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrRmToRegw = instrOp1ToOp2w

instrRegToRmw :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrRegToRmw = instrOp2ToOp1w

-------------------------------------------------------------------------------

emptyRegReg :: a -> a -> PrismM ()
emptyRegReg = M.emptyRegReg

emptySingle :: a -> PrismM ()
emptySingle = M.emptySingle

-------------------------------------------------------------------------------
