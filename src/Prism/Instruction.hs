{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Prism.Instruction where

import Data.Word (Word8, Word16)

import Prism.Cpu

-------------------------------------------------------------------------------

type OperandFunc1 a v = (Operand a PrismM v)
type OperandFunc2 a1 a2 v = (Operand a1 PrismM v, Operand a2 PrismM v)

--Plain function
--decoder is main user
type FuncImplicit = PrismM ()

--Takes immediate values
--decoder is main user
type FuncImm1 i = i -> PrismM ()
type FuncImm2 i = i -> i -> PrismM ()

--Takes and modifies operand
--executor is main user
type FuncO1M a = a -> PrismM ()
type FuncO2M a1 a2 = a1 -> a2 -> PrismM ()

--Takes immediate value + operand
--decoder is main user
type FuncOI1M a v = a -> v -> PrismM ()

--Takes and modifies value+monad from operand
--executor is main user
type FuncV1M v = v -> PrismM v
type FuncNV1M v = v -> PrismM ()

--Takes and modifies value from operand
--executor is main user
type FuncV1 v = v -> v
type FuncV2 v = v -> v -> v

--Takes and modifies value from operand
--executor is main user
type FuncVF1 v = Flags -> v -> (Flags, v)
type FuncVF2 v = Flags -> v -> v -> (Flags, v)

instrON1 :: (OperandFunc1 a v) => FuncNV1M v -> FuncO1M a
instrON1 func op =
    readOp op >>= func

{-# SPECIALISE instrON1 :: FuncNV1M Word8 -> FuncO1M Reg8 #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 -> FuncO1M Reg16 #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 -> FuncO1M RegSeg #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word8 -> FuncO1M MemSeg8 #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 -> FuncO1M MemSeg16 #-}

instrOM1 :: (OperandFunc1 a v) => FuncV1M v -> FuncO1M a
instrOM1 func op =
    readOp op >>= func >>= writeOp op

{-# SPECIALISE instrOM1 :: FuncV1M Word8 -> FuncO1M Reg8 #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word16 -> FuncO1M Reg16 #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word16 -> FuncO1M RegSeg #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word8 -> FuncO1M MemSeg8 #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word16 -> FuncO1M MemSeg16 #-}

instrO1 :: (OperandFunc1 a v) => FuncV1 v -> FuncO1M a
instrO1 func op = do
    (func <$> readOp op) >>= writeOp op

{-# SPECIALISE instrO1 :: FuncV1 Word8 -> FuncO1M Reg8 #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M Reg16 #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M RegSeg #-}
{-# SPECIALISE instrO1 :: FuncV1 Word8 -> FuncO1M MemSeg8 #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M MemSeg16 #-}

instrOI1 :: (OperandFunc1 a v) => FuncV2 v -> FuncOI1M a v
instrOI1 func op imm =
    (func imm <$> readOp op) >>= writeOp op

{-# SPECIALISE instrOI1 :: FuncV2 Word8 -> FuncOI1M Reg8 Word8 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M Reg16 Word16 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M RegSeg Word16 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word8 -> FuncOI1M MemSeg8 Word8 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M MemSeg16 Word16 #-}

instrOF1 :: (OperandFunc1 a v) => FuncVF1 v -> FuncO1M a
instrOF1 func op = do
    val <- readOp op
    flags <- getFlags
    let (newFlags, newVal) = func flags val
    setFlags newFlags
    writeOp op newVal

{-# SPECIALISE instrOF1 :: FuncVF1 Word8 -> FuncO1M Reg8 #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word16 -> FuncO1M Reg16 #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word16 -> FuncO1M RegSeg #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word8 -> FuncO1M MemSeg8 #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word16 -> FuncO1M MemSeg16 #-}

instrOFI1 :: (OperandFunc1 a v) => FuncVF2 v -> FuncOI1M a v
instrOFI1 func op imm = do
    val <- readOp op
    flags <- getFlags
    let (newFlags, newVal) = func flags imm val
    setFlags newFlags
    writeOp op newVal

{-# SPECIALISE instrOFI1 :: FuncVF2 Word8 -> FuncOI1M Reg8 Word8 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word16 -> FuncOI1M Reg16 Word16 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word16 -> FuncOI1M RegSeg Word16 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word8 -> FuncOI1M MemSeg8 Word8 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word16 -> FuncOI1M MemSeg16 Word16 #-}

instrOI1w :: (OperandFunc1 a v) => FuncV2 v -> FuncOI1M a v
instrOI1w func op imm =
    (return $ func imm 0) >>= writeOp op

{-# SPECIALISE instrOI1w :: FuncV2 Word8 -> FuncOI1M Reg8 Word8 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word16 -> FuncOI1M Reg16 Word16 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word16 -> FuncOI1M RegSeg Word16 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word8 -> FuncOI1M MemSeg8 Word8 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word16 -> FuncOI1M MemSeg16 Word16 #-}

instrO2 :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrO2 func op1 op2 =
    (func <$> (readOp op1) <*> (readOp op2)) >>= writeOp op2

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
instrO2w func op1 op2 =
    (func <$> (readOp op1) <*> (return 0)) >>= writeOp op2

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

instrOp1ToOp2 :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrOp1ToOp2 = instrO2

instrOp2ToOp1 :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrOp2ToOp1 func op1 op2 = instrO2 func op2 op1

instrRmToReg :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrRmToReg = instrOp1ToOp2

instrRegToRm :: (OperandFunc2 a1 a2 v) => FuncV2 v -> FuncO2M a1 a2
instrRegToRm = instrOp2ToOp1

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
emptyRegReg _ _ = return ()

emptySingle :: a -> PrismM ()
emptySingle _ = return ()

-------------------------------------------------------------------------------
