module Instruction.Arithmetic where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Bits (shiftR, shiftL, (.|.), (.&.))

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
        after = dest - source
        flags = Flags (calcCFBorrow8 dest after) (calcPF8 after) (calcAFBorrow8 dest after) (calcZF8 after) (calcSF8 after) (calcOFSub8 dest source after)
        newCtx = ctx { ctxFlags = flags }

sub16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)
sub16 ctx source dest = (newCtx, after)
    where
        after = dest - source
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

neg8 :: Ctx -> Uint8 -> (Ctx, Uint8)
neg8 ctx val = (newCtx, result)
    where
        result = 0 - val
        cf = val /= 0
        of_ = val == 0x80
        flags = Flags cf (calcPF8 result) (calcAFBorrow8 0 result) (calcZF8 result) (calcSF8 result) of_
        newCtx = ctx { ctxFlags = flags }

neg16 :: Ctx -> Uint16 -> (Ctx, Uint16)
neg16 ctx val = (newCtx, result)
    where
        result = 0 - val
        cf = val /= 0
        of_ = val == 0x8000
        flags = Flags cf (calcPF16 result) (calcAFBorrow16 0 result) (calcZF16 result) (calcSF16 result) of_
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

aaa :: Ctx -> Uint16 -> (Ctx, Uint16)
aaa ctx val = (newCtx, result)
    where
        alVal = val .&. 0xFF
        af = (flagAF . ctxFlags $ ctx)
        overflow = ((alVal .&. 0x0F) > 9) || af
        result = (if overflow then val + 0x106 else val) .&. 0xFF0F
        cf_ = overflow
        af_ = overflow
        flags = Flags cf_ False af_ False False False
        newCtx = ctx { ctxFlags = flags }

aad :: Ctx -> Uint16 -> (Ctx, Uint16)
aad ctx val = (newCtx, result)
    where
        alVal = val .&. 0xFF
        ahVal = shiftR val 8
        result = (alVal + ahVal * 10) .&. 0x00FF
        flags = Flags False (calcPF16 result) False (calcZF16 result) (calcSF16 result) False
        newCtx = ctx { ctxFlags = flags }

aam :: Ctx -> Uint16 -> (Ctx, Uint16)
aam ctx val = (newCtx, result)
    where
        alVal = val .&. 0xFF
        newAhVal = shiftL (div alVal 10) 8
        newAlVal = mod alVal 10
        result = newAhVal .|. newAlVal
        flags = Flags False (calcPF16 result) False (calcZF16 result) (calcSF16 result) False
        newCtx = ctx { ctxFlags = flags }

aas :: Ctx -> Uint16 -> (Ctx, Uint16)
aas ctx val = (newCtx, result)
    where
        alVal = val .&. 0xFF
        af = (flagAF . ctxFlags $ ctx)
        overflow = ((alVal .&. 0x0F) > 9) || af
        result = (if overflow then val - 0x106 else val) .&. 0xFF0F
        cf_ = overflow
        af_ = overflow
        flags = Flags cf_ False af_ False False False
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

daa :: Ctx -> Uint8 -> (Ctx, Uint8)
daa ctx val = (newCtx, result)
    where
        af = (flagAF . ctxFlags $ ctx)
        cf = (flagCF . ctxFlags $ ctx)
        overflowL = ((val .&. 0x0F) > 9) || af
        overflowH = (val > 0x99) || cf
        resultL = if overflowL then val + 6 else val
        result = if overflowH then resultL + 0x60 else resultL
        cf_ = overflowH || (calcCFCarry8 val resultL)
        af_ = overflowL
        flags = Flags cf_ False af_ False False False
        newCtx = ctx { ctxFlags = flags }

das :: Ctx -> Uint8 -> (Ctx, Uint8)
das ctx val = (newCtx, result)
    where
        af = (flagAF . ctxFlags $ ctx)
        cf = (flagCF . ctxFlags $ ctx)
        overflowL = ((val .&. 0x0F) > 9) || af
        overflowH = (val > 0x99) || cf
        resultL = if overflowL then val - 6 else val
        result = if overflowH then resultL - 0x60 else resultL
        cf_ = overflowH || (calcCFCarry8 val resultL)
        af_ = overflowL
        flags = Flags cf_ False af_ False False False
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

type MuldivFunc8 = Ctx -> Uint8 -> Uint8 -> (Ctx, Uint16)
type MuldivFunc16 = Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16, Uint16)

mul8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint16)
mul8 ctx val1 val2 = (newCtx, result)
    where
        result = (fromIntegral val1) * (fromIntegral val2)
        cf_ = result > 0x00FF
        of_ = cf_
        flags = Flags cf_ False False False False of_
        newCtx = ctx { ctxFlags = flags }

mul16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16, Uint16)
mul16 ctx val1 val2 = (newCtx, resultH, resultL)
    where
        result = ((fromIntegral val1) * (fromIntegral val2) :: Uint32)
        resultH = fromIntegral $ shiftR result 16
        resultL = fromIntegral result
        cf_ = resultH > 0
        of_ = cf_
        flags = Flags cf_ False False False False of_
        newCtx = ctx { ctxFlags = flags }

imul8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint16)
imul8 ctx val1 val2 = (newCtx, result)
    where
        result = (fromIntegral val1) * (fromIntegral val2)
        signExt = result .&. 0xFF00
        cf_ = signExt /= 0 && signExt /= 0xFF00
        of_ = cf_
        flags = Flags cf_ False False False False of_
        newCtx = ctx { ctxFlags = flags }

imul16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16, Uint16)
imul16 ctx val1 val2 = (newCtx, resultH, resultL)
    where
        result = ((fromIntegral val1) * (fromIntegral val2) :: Uint32)
        resultH = fromIntegral $ shiftR result 16
        resultL = fromIntegral result
        signExt = resultH /= 0xFFFF && resultH /= 0
        cf_ = signExt
        of_ = cf_
        flags = Flags cf_ False False False False of_
        newCtx = ctx { ctxFlags = flags }
{-
div8 :: Ctx -> Uint16 -> Uint8 -> (Ctx, Uint16)
div8 ctx val1 val2 = (ctx, result)
    where
        resultL = div val1 val2 
        resultH = mod val1 val2
        result = 
        -}

muldivInstr8 :: MuldivFunc8 -> FuncNoVal8
muldivInstr8 func ctx val = do
    alVal <- readReg8 memReg al
    let (newCtx, axVal) = func ctx alVal val
    writeReg16 memReg ax axVal
    return newCtx
    where
        memReg = ctxReg ctx

muldivInstr16 :: MuldivFunc16 -> FuncNoVal16
muldivInstr16 func ctx val = do
    axVal <- readReg16 memReg ax
    let (newCtx, dxVal, axVal) = func ctx axVal val
    writeReg16 memReg ax axVal
    writeReg16 memReg dx dxVal
    return newCtx
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
        --CMP
        makeInstructionS 0x38 Nothing (decodeRm8 (instrRegToReg8RegToRm cmp8) (instrRegToMem8 cmp8)),
        makeInstructionS 0x39 Nothing (decodeRm16 (instrRegToReg16RegToRm cmp16) (instrRegToMem16 cmp16)),
        makeInstructionS 0x3A Nothing (decodeRm8 (instrRegToReg8RmToReg cmp8) (instrMemToReg8 cmp8)),
        makeInstructionS 0x3B Nothing (decodeRm16 (instrRegToReg16RmToReg cmp16) (instrMemToReg16 cmp16)),
        makeInstructionS 0x3C Nothing (decodeAcc8 al $ instrRegImm8 cmp8),
        makeInstructionS 0x3D Nothing (decodeAcc16 ax $ instrRegImm16 cmp16),
        makeInstructionS 0x80 (Just 0x7) (decodeN8Imm8 (instrRegImm8 cmp8) (instrMemImm8 cmp8)),
        makeInstructionS 0x81 (Just 0x7) (decodeN16Imm (instrRegImm16 cmp16) (instrMemImm16 cmp16)),
        makeInstructionS 0x82 (Just 0x7) (decodeN8Imm8 (instrRegImm8 cmp8) (instrMemImm8 cmp8)),
        makeInstructionS 0x83 (Just 0x7) (decodeN16Imm8 (instrRegImm16 cmp16) (instrMemImm16 cmp16)),
        --NEG
        makeInstructionS 0xF6 (Just 3) (decodeN8 (instrReg8 neg8) (instrMem8 neg8)),
        makeInstructionS 0xF7 (Just 3) (decodeN16 (instrReg16 neg16) (instrMem16 neg16)),
        --AAA/AAD/AAM/AAS
        makeInstructionS 0x37 Nothing (decodeReg16 ax $ instrReg16 aaa),
        makeInstructionS 0xD5 (Just 1) (decodeReg16 ax $ instrReg16 aad),
        makeInstructionS 0xD4 (Just 1) (decodeReg16 ax $ instrReg16 aam),
        makeInstructionS 0x3F Nothing (decodeReg16 ax $ instrReg16 aas),
        --DAA/DAS
        makeInstructionS 0x27 Nothing (decodeReg8 al $ instrReg8 daa),
        makeInstructionS 0x2F Nothing (decodeReg8 al $ instrReg8 das),
        --MUL
        makeInstructionS 0xF6 (Just 4) (decodeN8 (instrRegNoVal8 $ muldivInstr8 mul8) (instrMemNoVal8 $ muldivInstr8 mul8)),
        makeInstructionS 0xF7 (Just 4) (decodeN16 (instrRegNoVal16 $ muldivInstr16 mul16) (instrMemNoVal16 $ muldivInstr16 mul16)),
        makeInstructionS 0xF6 (Just 5) (decodeN8 (instrRegNoVal8 $ muldivInstr8 imul8) (instrMemNoVal8 $ muldivInstr8 imul8)),
        makeInstructionS 0xF7 (Just 5) (decodeN16 (instrRegNoVal16 $ muldivInstr16 imul16) (instrMemNoVal16 $ muldivInstr16 imul16)),
        --CBW/CWD
        makeInstructionS 0x98 Nothing (decodeImplicit cbw),
        makeInstructionS 0x99 Nothing (decodeImplicit cwd)
    ]
