module Instruction.Arithmetic where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Bits (shiftR)

import Prism
import PrismDecoder
import PrismCpu

-------------------------------------------------------------------------------

add8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
add8 ctx source dest = (newCtx, after)
    where
        after = source + dest
        flags = Flags (calcCFCarry8 dest after) (calcPF8 after) (calcAFCarry8 dest after) (calcZF8 after) (calcSF8 after) (calcOFAdd8 dest source after)
        newCtx = ctx { ctxFlags = flags }

add16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)
add16 ctx source dest = (newCtx, after)
    where
        after = source + dest
        flags = Flags (calcCFCarry16 dest after) (calcPF16 after) (calcAFCarry16 dest after) (calcZF16 after) (calcSF16 after) (calcOFAdd16 dest source after)
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

adc8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
adc8 ctx source dest = add8 ctx source newDest
    where
        newDest = dest + if flagCF $ ctxFlags ctx then 1 else 0

adc16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)
adc16 ctx source dest = add16 ctx source newDest
    where
        newDest = dest + if flagCF $ ctxFlags ctx then 1 else 0

-------------------------------------------------------------------------------

inc8 :: Ctx -> Uint8 -> (Ctx, Uint8)
inc8 ctx val = (newCtx, result)
    where
        result = val + 1
        flags = Flags (flagCF . ctxFlags $ ctx) (calcPF8 result) (calcAFCarry8 val result) (calcZF8 result) (calcSF8 result) (calcOFAdd8 val 1 result)
        newCtx = ctx { ctxFlags = flags }

inc16 :: Ctx -> Uint16 -> (Ctx, Uint16)
inc16 ctx val = (newCtx, result)
    where
        result = val + 1
        flags = Flags (flagCF . ctxFlags $ ctx) (calcPF16 result) (calcAFCarry16 val result) (calcZF16 result) (calcSF16 result) (calcOFAdd16 val 1 result)
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

sub8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
sub8 ctx source dest = (newCtx, after)
    where
        after = source - dest
        flags = Flags (calcCFBorrow8 dest after) (calcPF8 after) (calcAFBorrow8 dest after) (calcZF8 after) (calcSF8 after) (calcOFSub8 dest source after)
        newCtx = ctx { ctxFlags = flags }

sub16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)
sub16 ctx source dest = (newCtx, after)
    where
        after = source - dest
        flags = Flags (calcCFBorrow16 dest after) (calcPF16 after) (calcAFBorrow16 dest after) (calcZF16 after) (calcSF16 after) (calcOFSub16 dest source after)
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

sbb8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
sbb8 ctx source dest = sub8 ctx source newDest
    where
        newDest = dest - if flagCF $ ctxFlags ctx then 1 else 0

sbb16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)
sbb16 ctx source dest = sub16 ctx source newDest
    where
        newDest = dest - if flagCF $ ctxFlags ctx then 1 else 0

-------------------------------------------------------------------------------

dec8 :: Ctx -> Uint8 -> (Ctx, Uint8)
dec8 ctx val = (newCtx, result)
    where
        result = val - 1
        flags = Flags (flagCF . ctxFlags $ ctx) (calcPF8 result) (calcAFBorrow8 val result) (calcZF8 result) (calcSF8 result) (calcOFSub8 val 1 result)
        newCtx = ctx { ctxFlags = flags }

dec16 :: Ctx -> Uint16 -> (Ctx, Uint16)
dec16 ctx val = (newCtx, result)
    where
        result = val - 1
        flags = Flags (flagCF . ctxFlags $ ctx) (calcPF16 result) (calcAFBorrow16 val result) (calcZF16 result) (calcSF16 result) (calcOFSub16 val 1 result)
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

cmp8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
cmp8 ctx source dest = (newCtx, dest)
    where
        (newCtx, _) = sub8 ctx source dest

cmp16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)
cmp16 ctx source dest = (newCtx, dest)
    where
        (newCtx, _) = sub16 ctx source dest

-------------------------------------------------------------------------------

cbw :: Ctx -> PrismM
cbw ctx = do
    val8 <- readReg8 memReg al
    let val16 = signExterndWord val8
    writeReg16 memReg ax val16
    return ctx
    where
        memReg = ctxReg ctx

cwd :: Ctx -> PrismM
cwd ctx = do
    val16 <- readReg16 memReg ax
    let (valH, valL) = signExterndDoubleword val16
    writeReg16 memReg ax valL
    writeReg16 memReg dx valH
    return ctx
    where
        memReg = ctxReg ctx

-------------------------------------------------------------------------------

arithmeticInstrList = [
        --ADD
        makeInstructionS 0x00 Nothing (decodeRm8 (instrRegToReg8RegToRm add8) (instrRegToMem8 add8)),
        makeInstructionS 0x01 Nothing (decodeRm16 (instrRegToReg16RegToRm add16) (instrRegToMem16 add16)),
        makeInstructionS 0x02 Nothing (decodeRm8 (instrRegToReg8RmToReg add8) (instrMemToReg8 add8)),
        makeInstructionS 0x03 Nothing (decodeRm16 (instrRegToReg16RmToReg add16) (instrMemToReg16 add16)),
        makeInstructionS 0x04 Nothing (decodeAcc8 al $ instrRegImm8 add8),
        makeInstructionS 0x05 Nothing (decodeAcc16 ax $ instrRegImm16 add16),
        makeInstructionS 0x80 (Just 0) (decodeN8Imm8 (instrRegImm8 add8) (instrMemImm8 add8)),
        makeInstructionS 0x81 (Just 0) (decodeN16Imm (instrRegImm16 add16) (instrMemImm16 add16)),
        makeInstructionS 0x82 (Just 0) (decodeN8Imm8 (instrRegImm8 add8) (instrMemImm8 add8)),
        makeInstructionS 0x83 (Just 0) (decodeN16Imm8 (instrRegImm16 add16) (instrMemImm16 add16)),
        --ADC
        makeInstructionS 0x10 Nothing (decodeRm8 (instrRegToReg8RegToRm adc8) (instrRegToMem8 adc8)),
        makeInstructionS 0x11 Nothing (decodeRm16 (instrRegToReg16RegToRm adc16) (instrRegToMem16 adc16)),
        makeInstructionS 0x12 Nothing (decodeRm8 (instrRegToReg8RmToReg adc8) (instrMemToReg8 adc8)),
        makeInstructionS 0x13 Nothing (decodeRm16 (instrRegToReg16RmToReg adc16) (instrMemToReg16 adc16)),
        makeInstructionS 0x14 Nothing (decodeAcc8 al $ instrRegImm8 adc8),
        makeInstructionS 0x15 Nothing (decodeAcc16 ax $ instrRegImm16 adc16),
        makeInstructionS 0x80 (Just 0x2) (decodeN8Imm8 (instrRegImm8 adc8) (instrMemImm8 adc8)),
        makeInstructionS 0x81 (Just 0x2) (decodeN16Imm (instrRegImm16 adc16) (instrMemImm16 adc16)),
        makeInstructionS 0x82 (Just 0x2) (decodeN8Imm8 (instrRegImm8 adc8) (instrMemImm8 adc8)),
        makeInstructionS 0x83 (Just 0x2) (decodeN16Imm8 (instrRegImm16 adc16) (instrMemImm16 adc16)),
        --INC
        makeInstructionS 0x40 Nothing (decodeReg16 ax (instrReg16 inc16)),
        makeInstructionS 0x41 Nothing (decodeReg16 cx (instrReg16 inc16)),
        makeInstructionS 0x42 Nothing (decodeReg16 dx (instrReg16 inc16)),
        makeInstructionS 0x43 Nothing (decodeReg16 bx (instrReg16 inc16)),
        makeInstructionS 0x44 Nothing (decodeReg16 sp (instrReg16 inc16)),
        makeInstructionS 0x45 Nothing (decodeReg16 bp (instrReg16 inc16)),
        makeInstructionS 0x46 Nothing (decodeReg16 si (instrReg16 inc16)),
        makeInstructionS 0x47 Nothing (decodeReg16 di (instrReg16 inc16)),
        makeInstructionS 0xFE (Just 0) (decodeN8 (instrReg8 inc8) (instrMem8 inc8)),
        makeInstructionS 0xFF (Just 0) (decodeN16 (instrReg16 inc16) (instrMem16 inc16)),
        --SUB
        makeInstructionS 0x28 Nothing (decodeRm8 (instrRegToReg8RegToRm sub8) (instrRegToMem8 sub8)),
        makeInstructionS 0x29 Nothing (decodeRm16 (instrRegToReg16RegToRm sub16) (instrRegToMem16 sub16)),
        makeInstructionS 0x2A Nothing (decodeRm8 (instrRegToReg8RmToReg sub8) (instrMemToReg8 sub8)),
        makeInstructionS 0x2B Nothing (decodeRm16 (instrRegToReg16RmToReg sub16) (instrMemToReg16 sub16)),
        makeInstructionS 0x2C Nothing (decodeAcc8 al $ instrRegImm8 sub8),
        makeInstructionS 0x2D Nothing (decodeAcc16 ax $ instrRegImm16 sub16),
        makeInstructionS 0x80 (Just 0x5) (decodeN8Imm8 (instrRegImm8 sub8) (instrMemImm8 sub8)),
        makeInstructionS 0x81 (Just 0x5) (decodeN16Imm (instrRegImm16 sub16) (instrMemImm16 sub16)),
        makeInstructionS 0x82 (Just 0x5) (decodeN8Imm8 (instrRegImm8 sub8) (instrMemImm8 sub8)),
        makeInstructionS 0x83 (Just 0x5) (decodeN16Imm8 (instrRegImm16 sub16) (instrMemImm16 sub16)),
        --SBB
        makeInstructionS 0x18 Nothing (decodeRm8 (instrRegToReg8RegToRm sbb8) (instrRegToMem8 sbb8)),
        makeInstructionS 0x19 Nothing (decodeRm16 (instrRegToReg16RegToRm sbb16) (instrRegToMem16 sbb16)),
        makeInstructionS 0x1A Nothing (decodeRm8 (instrRegToReg8RmToReg sbb8) (instrMemToReg8 sbb8)),
        makeInstructionS 0x1B Nothing (decodeRm16 (instrRegToReg16RmToReg sbb16) (instrMemToReg16 sbb16)),
        makeInstructionS 0x1C Nothing (decodeAcc8 al $ instrRegImm8 sbb8),
        makeInstructionS 0x1D Nothing (decodeAcc16 ax $ instrRegImm16 sbb16),
        makeInstructionS 0x80 (Just 0x3) (decodeN8Imm8 (instrRegImm8 sbb8) (instrMemImm8 sbb8)),
        makeInstructionS 0x81 (Just 0x3) (decodeN16Imm (instrRegImm16 sbb16) (instrMemImm16 sbb16)),
        makeInstructionS 0x82 (Just 0x3) (decodeN8Imm8 (instrRegImm8 sbb8) (instrMemImm8 sbb8)),
        makeInstructionS 0x83 (Just 0x3) (decodeN16Imm8 (instrRegImm16 sbb16) (instrMemImm16 sbb16)),
        --DEC
        makeInstructionS 0x48 Nothing (decodeReg16 ax (instrReg16 dec16)),
        makeInstructionS 0x49 Nothing (decodeReg16 cx (instrReg16 dec16)),
        makeInstructionS 0x4A Nothing (decodeReg16 dx (instrReg16 dec16)),
        makeInstructionS 0x4B Nothing (decodeReg16 bx (instrReg16 dec16)),
        makeInstructionS 0x4C Nothing (decodeReg16 sp (instrReg16 dec16)),
        makeInstructionS 0x4D Nothing (decodeReg16 bp (instrReg16 dec16)),
        makeInstructionS 0x4E Nothing (decodeReg16 si (instrReg16 dec16)),
        makeInstructionS 0x4F Nothing (decodeReg16 di (instrReg16 dec16)),
        makeInstructionS 0xFE (Just 1) (decodeN8 (instrReg8 dec8) (instrMem8 dec8)),
        makeInstructionS 0xFF (Just 1) (decodeN16 (instrReg16 dec16) (instrMem16 dec16)),
        --CBW/CWD
        makeInstructionS 0x98 Nothing (decodeImplicit cbw),
        makeInstructionS 0x99 Nothing (decodeImplicit cwd)
    ]
