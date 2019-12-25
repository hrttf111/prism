module Instruction.Arithmetic where

import Control.Monad.Trans (liftIO, MonadIO)
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.Int

import Prism
import PrismDecoder
import PrismCpu

-------------------------------------------------------------------------------

add :: OperandVal b => FuncV2 b
add ctx source dest = (newCtx, after)
    where
        after = source + dest
        flags = Flags (calcCFCarry dest after) (calcPF after) (calcAFCarry dest after) (calcZF after) (calcSF after) (calcOFAdd dest source after)
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

adc :: OperandVal b => FuncV2 b
adc ctx source dest = add ctx source newDest
    where
        newDest = dest + if flagCF $ ctxFlags ctx then 1 else 0

-------------------------------------------------------------------------------

inc :: OperandVal b => FuncV1 b
inc ctx val = (newCtx, result)
    where
        result = val + 1
        flags = Flags (flagCF . ctxFlags $ ctx) (calcPF result) (calcAFCarry val result) (calcZF result) (calcSF result) (calcOFAdd val 1 result)
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

sub :: OperandVal b => FuncV2 b
sub ctx source dest = (newCtx, after)
    where
        after = dest - source
        flags = Flags (calcCFBorrow dest after) (calcPF after) (calcAFBorrow dest after) (calcZF after) (calcSF after) (calcOFSub dest source after)
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

sbb :: OperandVal b => FuncV2 b
sbb ctx source dest = sub ctx source newDest
    where
        newDest = dest - if flagCF $ ctxFlags ctx then 1 else 0

-------------------------------------------------------------------------------

dec :: OperandVal b => FuncV1 b
dec ctx val = (newCtx, result)
    where
        result = val - 1
        flags = Flags (flagCF . ctxFlags $ ctx) (calcPF result) (calcAFBorrow val result) (calcZF result) (calcSF result) (calcOFSub val 1 result)
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

cmp :: OperandVal b => FuncV2 b
cmp ctx source dest = (newCtx, dest)
    where
        (newCtx, _) = sub ctx source dest

-------------------------------------------------------------------------------

cbw :: FuncImplicit
cbw ctx = do
    val8 <- readOp ctx al
    let val16 = signExterndWord val8
    writeOp ctx ax val16
    return ctx

cwd :: FuncImplicit
cwd ctx = do
    val16 <- readOp ctx ax
    let (valH, valL) = signExterndDoubleword val16
    writeOp ctx ax valL
    writeOp ctx dx valH
    return ctx

-------------------------------------------------------------------------------

neg :: OperandVal b => FuncV1 b
neg ctx val = (newCtx, result)
    where
        result = 0 - val
        cf = val /= 0
        of_ = val == negV
        flags = Flags cf (calcPF result) (calcAFBorrow 0 result) (calcZF result) (calcSF result) of_
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

aaa :: FuncV1 Uint16
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

aad :: FuncV1 Uint16
aad ctx val = (newCtx, result)
    where
        alVal = val .&. 0xFF
        ahVal = shiftR val 8
        result = (alVal + ahVal * 10) .&. 0x00FF
        flags = Flags False (calcPF16 result) False (calcZF16 result) (calcSF16 result) False
        newCtx = ctx { ctxFlags = flags }

aam :: FuncV1 Uint16
aam ctx val = (newCtx, result)
    where
        alVal = val .&. 0xFF
        newAhVal = shiftL (div alVal 10) 8
        newAlVal = mod alVal 10
        result = newAhVal .|. newAlVal
        flags = Flags False (calcPF16 result) False (calcZF16 result) (calcSF16 result) False
        newCtx = ctx { ctxFlags = flags }

aas :: FuncV1 Uint16
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

daa :: FuncV1 Uint8
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

das :: FuncV1 Uint8
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

type DivFunc8 = Ctx -> Uint16 -> Uint8 -> (Ctx, Uint16)
type DivFunc16 = Ctx -> Uint16 -> Uint16 -> Uint16 -> (Ctx, Uint16, Uint16)

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
        op = (*) :: Int16 -> Int16 -> Int16
        result = signedOp op (signExterndWord val1) (signExterndWord val2)
        signExt = result .&. 0xFF00
        cf_ = signExt /= 0 && signExt /= 0xFF00
        of_ = cf_
        flags = Flags cf_ False False False False of_
        newCtx = ctx { ctxFlags = flags }

imul16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16, Uint16)
imul16 ctx val1 val2 = (newCtx, resultH, resultL)
    where
        op = (*) :: Int32 -> Int32 -> Int32
        result = signedOp op (signExterndDoubleword32 val1) (signExterndDoubleword32 val2)
        resultH = fromIntegral $ shiftR result 16
        resultL = fromIntegral result
        signExt = resultH /= 0xFFFF && resultH /= 0
        cf_ = signExt
        of_ = cf_
        flags = Flags cf_ False False False False of_
        newCtx = ctx { ctxFlags = flags }

div8 :: Ctx -> Uint16 -> Uint8 -> (Ctx, Uint16)
div8 ctx val1 val2 = (ctx, result)
    where
        --todo: if val2 == 0 then DE
        val216 = (fromIntegral val2) :: Uint16
        resultL = div val1 val216
        resultH = mod val1 val216
        result = (resultL .&. 0x00FF) .|. (shiftL resultH 8)

div16 :: Ctx -> Uint16 -> Uint16 -> Uint16 -> (Ctx, Uint16, Uint16)
div16 ctx val1H val1L val2 = (ctx, resultH, resultL)
    where
        --todo: if val2 == 0 then DE
        val132 = ((shiftL (fromIntegral val1H) 16) .|. (fromIntegral val1L) :: Uint32)
        val232 = fromIntegral val2
        resultL = fromIntegral $ div val132 val232
        resultH = fromIntegral $ mod val132 $ fromIntegral val232

idiv8 :: Ctx -> Uint16 -> Uint8 -> (Ctx, Uint16)
idiv8 ctx val1 val2 = (ctx, result)
    where
        --todo: if val2 == 0 then DE
        opMod = mod :: Int16 -> Int16 -> Int16
        opDiv = div :: Int16 -> Int16 -> Int16
        val216 = signExterndWord val2 :: Uint16
        resultL = signedOp opDiv val1 val216
        resultH = signedOp opMod val1 val216
        result = (resultL .&. 0x00FF) .|. (shiftL resultH 8)

idiv16 :: Ctx -> Uint16 -> Uint16 -> Uint16 -> (Ctx, Uint16, Uint16)
idiv16 ctx val1H val1L val2 = (ctx, resultH, resultL)
    where
        --todo: if val2 == 0 then DE
        opMod = mod :: Int32 -> Int32 -> Int32
        opDiv = div :: Int32 -> Int32 -> Int32
        val132 = ((shiftL (fromIntegral val1H) 16) .|. (fromIntegral val1L) :: Uint32)
        val232 = signExterndDoubleword32 val2
        resultL = fromIntegral $ signedOp opDiv val132 val232
        resultH = fromIntegral $ signedOp opMod val132 val232

muldivInstr8 :: MuldivFunc8 -> FuncNV1M Uint8
muldivInstr8 func ctx val = do
    alVal <- readOp ctx al
    let (newCtx, axVal) = func ctx alVal val
    writeOp ctx ax axVal
    return newCtx

muldivInstr16 :: MuldivFunc16 -> FuncNV1M Uint16
muldivInstr16 func ctx val = do
    axVal <- readOp ctx ax
    let (newCtx, dxValNew, axValNew) = func ctx axVal val
    writeOp ctx ax axValNew
    writeOp ctx dx dxValNew
    return newCtx

divInstr8 :: DivFunc8 -> FuncNV1M Uint8
divInstr8 func ctx val = do
    axVal <- readOp ctx ax
    let (newCtx, axValNew) = func ctx axVal val
    writeOp ctx ax axValNew
    return newCtx

divInstr16 :: DivFunc16 -> FuncNV1M Uint16
divInstr16 func ctx val = do
    axVal <- readOp ctx ax
    dxVal <- readOp ctx dx
    let (newCtx, dxValNew, axValNew) = func ctx dxVal axVal val
    writeOp ctx ax axValNew
    writeOp ctx dx dxValNew
    return newCtx

-------------------------------------------------------------------------------

arithmeticInstrList = [
        --ADD
        makeInstructionS 0x00 Nothing (decodeRM8 (instrRegToRm add) (instrRegToRm add)),
        makeInstructionS 0x01 Nothing (decodeRM16 (instrRegToRm add) (instrRegToRm add)),
        makeInstructionS 0x02 Nothing (decodeRM8 (instrRmToReg add) (instrRmToReg add)),
        makeInstructionS 0x03 Nothing (decodeRM16 (instrRmToReg add) (instrRmToReg add)),
        makeInstructionS 0x04 Nothing (decodeStRI al $ instrOI1 add),
        makeInstructionS 0x05 Nothing (decodeStRI ax $ instrOI1 add),
        makeInstructionS 0x80 (Just 0) (decodeNI8 (instrOI1 add) (instrOI1 add)),
        makeInstructionS 0x81 (Just 0) (decodeNI16 (instrOI1 add) (instrOI1 add)),
        makeInstructionS 0x82 (Just 0) (decodeNI8 (instrOI1 add) (instrOI1 add)),
        makeInstructionS 0x83 (Just 0) (decodeNC16 (instrOI1 add) (instrOI1 add)),
        --ADC
        makeInstructionS 0x10 Nothing (decodeRM8 (instrRegToRm adc) (instrRegToRm adc)),
        makeInstructionS 0x11 Nothing (decodeRM16 (instrRegToRm adc) (instrRegToRm adc)),
        makeInstructionS 0x12 Nothing (decodeRM8 (instrRmToReg adc) (instrRmToReg adc)),
        makeInstructionS 0x13 Nothing (decodeRM16 (instrRmToReg adc) (instrRmToReg adc)),
        makeInstructionS 0x14 Nothing (decodeStRI al $ instrOI1 adc),
        makeInstructionS 0x15 Nothing (decodeStRI ax $ instrOI1 adc),
        makeInstructionS 0x80 (Just 0x2) (decodeNI8 (instrOI1 adc) (instrOI1 adc)),
        makeInstructionS 0x81 (Just 0x2) (decodeNI16 (instrOI1 adc) (instrOI1 adc)),
        makeInstructionS 0x82 (Just 0x2) (decodeNI8 (instrOI1 adc) (instrOI1 adc)),
        makeInstructionS 0x83 (Just 0x2) (decodeNC16 (instrOI1 adc) (instrOI1 adc)),
        --INC
        makeInstructionS 0x40 Nothing (decodeStR ax (instrO1 inc)),
        makeInstructionS 0x41 Nothing (decodeStR cx (instrO1 inc)),
        makeInstructionS 0x42 Nothing (decodeStR dx (instrO1 inc)),
        makeInstructionS 0x43 Nothing (decodeStR bx (instrO1 inc)),
        makeInstructionS 0x44 Nothing (decodeStR sp (instrO1 inc)),
        makeInstructionS 0x45 Nothing (decodeStR bp (instrO1 inc)),
        makeInstructionS 0x46 Nothing (decodeStR si (instrO1 inc)),
        makeInstructionS 0x47 Nothing (decodeStR di (instrO1 inc)),
        makeInstructionS 0xFE (Just 0) (decodeN8 (instrO1 inc) (instrO1 inc)),
        makeInstructionS 0xFF (Just 0) (decodeN16 (instrO1 inc) (instrO1 inc)),
        --SUB
        makeInstructionS 0x28 Nothing (decodeRM8 (instrRegToRm sub) (instrRegToRm sub)),
        makeInstructionS 0x29 Nothing (decodeRM16 (instrRegToRm sub) (instrRegToRm sub)),
        makeInstructionS 0x2A Nothing (decodeRM8 (instrRmToReg sub) (instrRmToReg sub)),
        makeInstructionS 0x2B Nothing (decodeRM16 (instrRmToReg sub) (instrRmToReg sub)),
        makeInstructionS 0x2C Nothing (decodeStRI al $ instrOI1 sub),
        makeInstructionS 0x2D Nothing (decodeStRI ax $ instrOI1 sub),
        makeInstructionS 0x80 (Just 0x5) (decodeNI8 (instrOI1 sub) (instrOI1 sub)),
        makeInstructionS 0x81 (Just 0x5) (decodeNI16 (instrOI1 sub) (instrOI1 sub)),
        makeInstructionS 0x82 (Just 0x5) (decodeNI8 (instrOI1 sub) (instrOI1 sub)),
        makeInstructionS 0x83 (Just 0x5) (decodeNC16 (instrOI1 sub) (instrOI1 sub)),
        --SBB
        makeInstructionS 0x18 Nothing (decodeRM8 (instrRegToRm sbb) (instrRegToRm sbb)),
        makeInstructionS 0x19 Nothing (decodeRM16 (instrRegToRm sbb) (instrRegToRm sbb)),
        makeInstructionS 0x1A Nothing (decodeRM8 (instrRmToReg sbb) (instrRmToReg sbb)),
        makeInstructionS 0x1B Nothing (decodeRM16 (instrRmToReg sbb) (instrRmToReg sbb)),
        makeInstructionS 0x1C Nothing (decodeStRI al $ instrOI1 sbb),
        makeInstructionS 0x1D Nothing (decodeStRI ax $ instrOI1 sbb),
        makeInstructionS 0x80 (Just 0x3) (decodeNI8 (instrOI1 sbb) (instrOI1 sbb)),
        makeInstructionS 0x81 (Just 0x3) (decodeNI16 (instrOI1 sbb) (instrOI1 sbb)),
        makeInstructionS 0x82 (Just 0x3) (decodeNI8 (instrOI1 sbb) (instrOI1 sbb)),
        makeInstructionS 0x83 (Just 0x3) (decodeNC16 (instrOI1 sbb) (instrOI1 sbb)),
        --DEC
        makeInstructionS 0x48 Nothing (decodeStR ax (instrO1 dec)),
        makeInstructionS 0x49 Nothing (decodeStR cx (instrO1 dec)),
        makeInstructionS 0x4A Nothing (decodeStR dx (instrO1 dec)),
        makeInstructionS 0x4B Nothing (decodeStR bx (instrO1 dec)),
        makeInstructionS 0x4C Nothing (decodeStR sp (instrO1 dec)),
        makeInstructionS 0x4D Nothing (decodeStR bp (instrO1 dec)),
        makeInstructionS 0x4E Nothing (decodeStR si (instrO1 dec)),
        makeInstructionS 0x4F Nothing (decodeStR di (instrO1 dec)),
        makeInstructionS 0xFE (Just 1) (decodeN8 (instrO1 dec) (instrO1 dec)),
        makeInstructionS 0xFF (Just 1) (decodeN16 (instrO1 dec) (instrO1 dec)),
        --CMP
        makeInstructionS 0x38 Nothing (decodeRM8 (instrRegToRm cmp) (instrRegToRm cmp)),
        makeInstructionS 0x39 Nothing (decodeRM16 (instrRegToRm cmp) (instrRegToRm cmp)),
        makeInstructionS 0x3A Nothing (decodeRM8 (instrRmToReg cmp) (instrRmToReg cmp)),
        makeInstructionS 0x3B Nothing (decodeRM16 (instrRmToReg cmp) (instrRmToReg cmp)),
        makeInstructionS 0x3C Nothing (decodeStRI al $ instrOI1 cmp),
        makeInstructionS 0x3D Nothing (decodeStRI ax $ instrOI1 cmp),
        makeInstructionS 0x80 (Just 0x7) (decodeNI8 (instrOI1 cmp) (instrOI1 cmp)),
        makeInstructionS 0x81 (Just 0x7) (decodeNI16 (instrOI1 cmp) (instrOI1 cmp)),
        makeInstructionS 0x82 (Just 0x7) (decodeNI8 (instrOI1 cmp) (instrOI1 cmp)),
        makeInstructionS 0x83 (Just 0x7) (decodeNC16 (instrOI1 cmp) (instrOI1 cmp)),
        --NEG
        makeInstructionS 0xF6 (Just 3) (decodeN8 (instrO1 neg) (instrO1 neg)),
        makeInstructionS 0xF7 (Just 3) (decodeN16 (instrO1 neg) (instrO1 neg)),
        --AAA/AAD/AAM/AAS
        makeInstructionS 0x37 Nothing (decodeStR ax $ instrO1 aaa),
        makeInstructionS 0xD5 (Just 1) (decodeStR ax $ instrO1 aad),
        makeInstructionS 0xD4 (Just 1) (decodeStR ax $ instrO1 aam),
        makeInstructionS 0x3F Nothing (decodeStR ax $ instrO1 aas),
        --DAA/DAS
        makeInstructionS 0x27 Nothing (decodeStR al $ instrO1 daa),
        makeInstructionS 0x2F Nothing (decodeStR al $ instrO1 das),
        --MUL
        makeInstructionS 0xF6 (Just 4) (decodeN8 (instrON1 $ muldivInstr8 mul8) (instrON1 $ muldivInstr8 mul8)),
        makeInstructionS 0xF7 (Just 4) (decodeN16 (instrON1 $ muldivInstr16 mul16) (instrON1 $ muldivInstr16 mul16)),
        makeInstructionS 0xF6 (Just 5) (decodeN8 (instrON1 $ muldivInstr8 imul8) (instrON1 $ muldivInstr8 imul8)),
        makeInstructionS 0xF7 (Just 5) (decodeN16 (instrON1 $ muldivInstr16 imul16) (instrON1 $ muldivInstr16 imul16)),
        --DIV
        makeInstructionS 0xF6 (Just 6) (decodeN8 (instrON1 $ divInstr8 div8) (instrON1 $ divInstr8 div8)),
        makeInstructionS 0xF7 (Just 6) (decodeN16 (instrON1 $ divInstr16 div16) (instrON1 $ divInstr16 div16)),
        makeInstructionS 0xF6 (Just 7) (decodeN8 (instrON1 $ divInstr8 idiv8) (instrON1 $ divInstr8 idiv8)),
        makeInstructionS 0xF7 (Just 7) (decodeN16 (instrON1 $ divInstr16 idiv16) (instrON1 $ divInstr16 idiv16)),
        --CBW/CWD
        makeInstructionS 0x98 Nothing (decodeImplicit cbw),
        makeInstructionS 0x99 Nothing (decodeImplicit cwd)
    ]
