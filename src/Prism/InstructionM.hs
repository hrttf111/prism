{-# LANGUAGE ConstraintKinds #-}

module Prism.InstructionM where

import Data.Word (Word8, Word16)

import Prism.Cpu

-------------------------------------------------------------------------------

type OperandFunc1 a m v = (Operand a m v)
type OperandFunc2 a1 a2 m v = (Operand a1 m v, Operand a2 m v)

--Plain function
--decoder is main user
type FuncImplicit m = m ()

--Takes immediate values
--decoder is main user
type FuncImm1 i m = i -> m ()
type FuncImm2 i m = i -> i -> m ()

--Takes and modifies operand
--executor is main user
type FuncO1M a m = a -> m ()
type FuncO2M a1 a2 m = a1 -> a2 -> m ()

--Takes immediate value + operand
--decoder is main user
type FuncOI1M a m v = a -> v -> m ()

--Takes and modifies value+monad from operand
--executor is main user
type FuncV1M v m = v -> m v
type FuncNV1M v m = v -> m ()

--Takes and modifies value from operand
--executor is main user
type FuncV1 v = v -> v
type FuncV2 v = v -> v -> v

--Takes and modifies value from operand
--executor is main user
type FuncVF1 v = Flags -> v -> (Flags, v)
type FuncVF2 v = Flags -> v -> v -> (Flags, v)

-------------------------------------------------------------------------------

instrON1 :: (CpuMonad m, OperandFunc1 a m v) => FuncNV1M v m -> FuncO1M a m
instrON1 func op =
    readOp op >>= func

{-# SPECIALISE instrON1 :: FuncNV1M Word8 PrismM -> FuncO1M Reg8 PrismM #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 PrismM -> FuncO1M Reg16 PrismM #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 PrismM -> FuncO1M RegSeg PrismM #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word8 PrismM -> FuncO1M MemSeg8 PrismM #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 PrismM -> FuncO1M MemSeg16 PrismM #-}

instrOM1 :: (CpuMonad m, OperandFunc1 a m v) => FuncV1M v m -> FuncO1M a m
instrOM1 func op =
    readOp op >>= func >>= writeOp op

{-# SPECIALISE instrOM1 :: FuncV1M Word8 PrismM -> FuncO1M Reg8 PrismM #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word16 PrismM -> FuncO1M Reg16 PrismM #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word16 PrismM -> FuncO1M RegSeg PrismM #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word8 PrismM -> FuncO1M MemSeg8 PrismM #-}
{-# SPECIALISE instrOM1 :: FuncV1M Word16 PrismM -> FuncO1M MemSeg16 PrismM #-}

-------------------------------------------------------------------------------

instrO1 :: (CpuMonad m, OperandFunc1 a m v) => FuncV1 v -> FuncO1M a m
instrO1 func op = do
    (func <$> readOp op) >>= writeOp op

{-# SPECIALISE instrO1 :: FuncV1 Word8 -> FuncO1M Reg8 PrismM #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M Reg16 PrismM #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M RegSeg PrismM #-}
{-# SPECIALISE instrO1 :: FuncV1 Word8 -> FuncO1M MemSeg8 PrismM #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M MemSeg16 PrismM #-}

instrOI1 :: (CpuMonad m, OperandFunc1 a m v) => FuncV2 v -> FuncOI1M a m v
instrOI1 func op imm =
    (func imm <$> readOp op) >>= writeOp op

{-# SPECIALISE instrOI1 :: FuncV2 Word8 -> FuncOI1M Reg8 PrismM Word8 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M Reg16 PrismM Word16 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M RegSeg PrismM Word16 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word8 -> FuncOI1M MemSeg8 PrismM Word8 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M MemSeg16 PrismM Word16 #-}

instrOI1w :: (CpuMonad m, OperandFunc1 a m v) => FuncV2 v -> FuncOI1M a m v
instrOI1w func op imm =
    (return $ func imm 0) >>= writeOp op

{-# SPECIALISE instrOI1w :: FuncV2 Word8 -> FuncOI1M Reg8 PrismM Word8 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word16 -> FuncOI1M Reg16 PrismM Word16 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word16 -> FuncOI1M RegSeg PrismM Word16 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word8 -> FuncOI1M MemSeg8 PrismM Word8 #-}
{-# SPECIALISE instrOI1w :: FuncV2 Word16 -> FuncOI1M MemSeg16 PrismM Word16 #-}

instrO2 :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncV2 v -> FuncO2M a1 a2 m
instrO2 func op1 op2 =
    (func <$> (readOp op1) <*> (readOp op2)) >>= writeOp op2

{-# SPECIALISE instrO2 :: FuncV2 Word8 -> FuncO2M Reg8 Reg8 PrismM #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M Reg16 Reg16 PrismM #-}
{-# SPECIALISE instrO2 :: FuncV2 Word8 -> FuncO2M MemSeg8 Reg8 PrismM #-}
{-# SPECIALISE instrO2 :: FuncV2 Word8 -> FuncO2M Reg8 MemSeg8 PrismM #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M Reg16 MemSeg16 PrismM #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M MemSeg16 Reg16 PrismM #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M RegSeg Reg16 PrismM #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M Reg16 RegSeg PrismM #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M RegSeg MemSeg16 PrismM #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M MemSeg16 RegSeg PrismM #-}

instrO2w :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncV2 v -> FuncO2M a1 a2 m
instrO2w func op1 op2 =
    (func <$> (readOp op1) <*> (return 0)) >>= writeOp op2

{-# SPECIALISE instrO2w :: FuncV2 Word8 -> FuncO2M Reg8 Reg8 PrismM #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M Reg16 Reg16 PrismM #-}
{-# SPECIALISE instrO2w :: FuncV2 Word8 -> FuncO2M MemSeg8 Reg8 PrismM #-}
{-# SPECIALISE instrO2w :: FuncV2 Word8 -> FuncO2M Reg8 MemSeg8 PrismM #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M Reg16 MemSeg16 PrismM #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M MemSeg16 Reg16 PrismM #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M RegSeg Reg16 PrismM #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M Reg16 RegSeg PrismM #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M RegSeg MemSeg16 PrismM #-}
{-# SPECIALISE instrO2w :: FuncV2 Word16 -> FuncO2M MemSeg16 RegSeg PrismM #-}

-------------------------------------------------------------------------------

instrOF1 :: (CpuMonad m, OperandFunc1 a m v) => FuncVF1 v -> FuncO1M a m
instrOF1 func op = do
    val <- readOp op
    flags <- getFlags
    let (newFlags, newVal) = func flags val
    setFlags newFlags
    writeOp op newVal

{-# SPECIALISE instrOF1 :: FuncVF1 Word8 -> FuncO1M Reg8 PrismM #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word16 -> FuncO1M Reg16 PrismM #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word16 -> FuncO1M RegSeg PrismM #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word8 -> FuncO1M MemSeg8 PrismM #-}
{-# SPECIALISE instrOF1 :: FuncVF1 Word16 -> FuncO1M MemSeg16 PrismM #-}

instrOFI1 :: (CpuMonad m, OperandFunc1 a m v) => FuncVF2 v -> FuncOI1M a m v
instrOFI1 func op imm = do
    val <- readOp op
    flags <- getFlags
    let (newFlags, newVal) = func flags imm val
    setFlags newFlags
    writeOp op newVal

{-# SPECIALISE instrOFI1 :: FuncVF2 Word8 -> FuncOI1M Reg8 PrismM Word8 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word16 -> FuncOI1M Reg16 PrismM Word16 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word16 -> FuncOI1M RegSeg PrismM Word16 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word8 -> FuncOI1M MemSeg8 PrismM Word8 #-}
{-# SPECIALISE instrOFI1 :: FuncVF2 Word16 -> FuncOI1M MemSeg16 PrismM Word16 #-}

instrOF2 :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncVF2 v -> FuncO2M a1 a2 m
instrOF2 func op1 op2 = do
    val1 <- readOp op1
    val2 <- readOp op2
    flags <- getFlags
    let (newFlags, newVal) = func flags val1 val2
    setFlags newFlags
    writeOp op2 newVal

{-# SPECIALISE instrOF2 :: FuncVF2 Word8 -> FuncO2M Reg8 Reg8 PrismM #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M Reg16 Reg16 PrismM #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word8 -> FuncO2M MemSeg8 Reg8 PrismM #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word8 -> FuncO2M Reg8 MemSeg8 PrismM #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M Reg16 MemSeg16 PrismM #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M MemSeg16 Reg16 PrismM #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M RegSeg Reg16 PrismM #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M Reg16 RegSeg PrismM #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M RegSeg MemSeg16 PrismM #-}
{-# SPECIALISE instrOF2 :: FuncVF2 Word16 -> FuncO2M MemSeg16 RegSeg PrismM #-}

-------------------------------------------------------------------------------

instrOp1ToOp2 :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncV2 v -> FuncO2M a1 a2 m
instrOp1ToOp2 = instrO2

instrOp2ToOp1 :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncV2 v -> FuncO2M a1 a2 m
instrOp2ToOp1 func op1 op2 = instrO2 func op2 op1

instrRmToReg :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncV2 v -> FuncO2M a1 a2 m
instrRmToReg = instrOp1ToOp2

instrRegToRm :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncV2 v -> FuncO2M a1 a2 m
instrRegToRm = instrOp2ToOp1

-------------------------------------------------------------------------------

instrOp1ToOp2F :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncVF2 v -> FuncO2M a1 a2 m
instrOp1ToOp2F = instrOF2

instrOp2ToOp1F :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncVF2 v -> FuncO2M a1 a2 m
instrOp2ToOp1F func op1 op2 = instrOF2 func op2 op1

instrRmToRegF :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncVF2 v -> FuncO2M a1 a2 m
instrRmToRegF = instrOp1ToOp2F

instrRegToRmF :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncVF2 v -> FuncO2M a1 a2 m
instrRegToRmF = instrOp2ToOp1F

-------------------------------------------------------------------------------

instrOp1ToOp2w :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncV2 v -> FuncO2M a1 a2 m
instrOp1ToOp2w = instrO2w

instrOp2ToOp1w :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncV2 v -> FuncO2M a1 a2 m
instrOp2ToOp1w func op1 op2 = instrO2w func op2 op1

instrRmToRegw :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncV2 v -> FuncO2M a1 a2 m
instrRmToRegw = instrOp1ToOp2w

instrRegToRmw :: (CpuMonad m, OperandFunc2 a1 a2 m v) => FuncV2 v -> FuncO2M a1 a2 m
instrRegToRmw = instrOp2ToOp1w

-------------------------------------------------------------------------------

emptyRegReg :: (CpuMonad m) => a -> a -> m ()
emptyRegReg _ _ = return ()

emptySingle :: (CpuMonad m) => a -> m ()
emptySingle _ = return ()

-------------------------------------------------------------------------------
