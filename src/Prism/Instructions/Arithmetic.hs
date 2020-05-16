module Prism.Instructions.Arithmetic where

import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.Int

import Prism.Cpu
import Prism.Instruction

-------------------------------------------------------------------------------

add :: (OperandVal v) => FuncVF2 v
add _ source dest = (flags_, after)
    where
        after = source + dest
        flags_ = Flags (calcCFCarry dest after)
                       (calcPF after)
                       (calcAFCarry dest after)
                       (calcZF after)
                       (calcSF after)
                       (calcOFAdd dest source after)

-------------------------------------------------------------------------------

adc :: (OperandVal v) => FuncVF2 v
adc flags source dest = add flags source newDest
    where
        newDest = dest + if flagCF flags then 1 else 0

-------------------------------------------------------------------------------

inc :: (OperandVal v) => FuncVF1 v
inc flags val = (flags_, result)
    where
        result = val + 1
        flags_ = Flags (flagCF flags)
                       (calcPF result)
                       (calcAFCarry val result)
                       (calcZF result)
                       (calcSF result)
                       (calcOFAdd val 1 result)

-------------------------------------------------------------------------------

sub :: (OperandVal v) => FuncVF2 v
sub _ source dest = (flags_, after)
    where
        after = dest - source
        flags_ = Flags (calcCFBorrow dest after)
                       (calcPF after)
                       (calcAFBorrow dest after)
                       (calcZF after)
                       (calcSF after)
                       (calcOFSub dest source after)

-------------------------------------------------------------------------------

sbb :: (OperandVal v) => FuncVF2 v
sbb flags source dest = sub flags source newDest
    where
        newDest = dest - if flagCF flags then 1 else 0

-------------------------------------------------------------------------------

dec :: (OperandVal v) => FuncVF1 v
dec flags val = (flags_, result)
    where
        result = val - 1
        flags_ = Flags (flagCF flags)
                       (calcPF result)
                       (calcAFBorrow val result)
                       (calcZF result)
                       (calcSF result)
                       (calcOFSub val 1 result)

-------------------------------------------------------------------------------

cmp :: (OperandVal v) => FuncVF2 v
cmp flags source dest = (flags_, source)
    where
        (flags_, _) = sub flags source dest

-------------------------------------------------------------------------------

cbw :: FuncImplicit
cbw = do
    val8 <- readOp al
    let val16 = signExtendWord val8
    writeOp ax val16

cwd :: FuncImplicit
cwd = do
    val16 <- readOp ax
    let (valH, valL) = signExtendDoubleword val16
    writeOp ax valL
    writeOp dx valH

-------------------------------------------------------------------------------

neg :: (OperandVal v) => FuncVF1 v
neg flags val = (flags_, result)
    where
        result = 0 - val
        cf = val /= 0
        of_ = val == negV
        flags_ = Flags cf
                       (calcPF result)
                       (calcAFBorrow 0 result)
                       (calcZF result)
                       (calcSF result)
                       of_

-------------------------------------------------------------------------------

aaa :: FuncVF1 Uint16
aaa flags val = (flags_, result)
    where
        alVal = val .&. 0xFF
        af = (flagAF flags)
        overflow = ((alVal .&. 0x0F) > 9) || af
        result = (if overflow then val + 0x106 else val) .&. 0xFF0F
        cf_ = overflow
        af_ = overflow
        flags_ = Flags cf_ False af_ False False False

aad :: FuncVF1 Uint16
aad flags val = (flags_, result)
    where
        alVal = val .&. 0xFF
        ahVal = shiftR val 8
        result = (alVal + ahVal * 10) .&. 0x00FF
        flags_ = Flags False 
                       (calcPF result)
                       False
                       (calcZF result)
                       (calcSF result)
                       False

aam :: FuncVF1 Uint16
aam flags val = (flags_, result)
    where
        alVal = val .&. 0xFF
        newAhVal = shiftL (div alVal 10) 8
        newAlVal = mod alVal 10
        result = newAhVal .|. newAlVal
        flags_ = Flags False
                       (calcPF result)
                       False
                       (calcZF result)
                       (calcSF result)
                       False

aas :: FuncVF1 Uint16
aas flags val = (flags_, result)
    where
        alVal = val .&. 0xFF
        af = (flagAF flags)
        overflow = ((alVal .&. 0x0F) > 9) || af
        result = (if overflow then val - 0x106 else val) .&. 0xFF0F
        cf_ = overflow
        af_ = overflow
        flags_ = Flags cf_ False af_ False False False

-------------------------------------------------------------------------------

daa :: FuncVF1 Uint8
daa flags val = (flags_, result)
    where
        af = flagAF flags
        cf = flagCF flags
        overflowL = ((val .&. 0x0F) > 9) || af
        overflowH = (val > 0x99) || cf
        resultL = if overflowL then val + 6 else val
        result = if overflowH then resultL + 0x60 else resultL
        cf_ = overflowH || (calcCFCarry val resultL)
        af_ = overflowL
        flags_ = Flags cf_ False af_ False False False

das :: FuncVF1 Uint8
das flags val = (flags_, result)
    where
        af = flagAF flags
        cf = flagCF flags
        overflowL = ((val .&. 0x0F) > 9) || af
        overflowH = (val > 0x99) || cf
        resultL = if overflowL then val - 6 else val
        result = if overflowH then resultL - 0x60 else resultL
        cf_ = overflowH || (calcCFCarry val resultL)
        af_ = overflowL
        flags_ = Flags cf_ False af_ False False False

-------------------------------------------------------------------------------
{-
type MuldivFunc8 = Flags -> Uint8 -> Uint8 -> (Flags, Uint16)
type MuldivFunc16 = Flags -> Uint16 -> Uint16 -> (Flags, Uint16, Uint16)

type DivFunc8 = Flags -> Uint16 -> Uint8 -> (Flags, Uint16)
type DivFunc16 = Flags -> Uint16 -> Uint16 -> Uint16 -> (Flags, Uint16, Uint16)

mul8 :: MuldivFunc8
mul8 flags val1 val2 = (flags_, result)
    where
        result = (fromIntegral val1) * (fromIntegral val2)
        cf_ = result > 0x00FF
        of_ = cf_
        flags_ = Flags cf_ False False False False of_

mul16 :: MuldivFunc16
mul16 flags val1 val2 = (flags_, resultH, resultL)
    where
        result = ((fromIntegral val1) * (fromIntegral val2) :: Uint32)
        resultH = fromIntegral $ shiftR result 16
        resultL = fromIntegral result
        cf_ = resultH > 0
        of_ = cf_
        flags_ = Flags cf_ False False False False of_

imul8 :: MuldivFunc8
imul8 flags val1 val2 = (flags_, result)
    where
        op = (*) :: Int16 -> Int16 -> Int16
        result = signedOp op (signExtendWord val1) (signExtendWord val2)
        signExt = result .&. 0xFF00
        cf_ = signExt /= 0 && signExt /= 0xFF00
        of_ = cf_
        flags_ = Flags cf_ False False False False of_

imul16 :: MuldivFunc16
imul16 flags val1 val2 = (flags_, resultH, resultL)
    where
        op = (*) :: Int32 -> Int32 -> Int32
        result = signedOp op (signExtendDoubleword32 val1) (signExtendDoubleword32 val2)
        resultH = fromIntegral $ shiftR result 16
        resultL = fromIntegral result
        signExt = resultH /= 0xFFFF && resultH /= 0
        cf_ = signExt
        of_ = cf_
        flags_ = Flags cf_ False False False False of_

div8 :: MuldivFunc8
div8 flags val1 val2 = (flags, result)
    where
        --todo: if val2 == 0 then DE
        val216 = (fromIntegral val2) :: Uint16
        resultL = div val1 val216
        resultH = mod val1 val216
        result = (resultL .&. 0x00FF) .|. (shiftL resultH 8)

div16 :: MuldivFunc16
div16 flags val1H val1L val2 = (flags, resultH, resultL)
    where
        --todo: if val2 == 0 then DE
        val132 = ((shiftL (fromIntegral val1H) 16) .|. (fromIntegral val1L) :: Uint32)
        val232 = fromIntegral val2
        resultL = fromIntegral $ div val132 val232
        resultH = fromIntegral $ mod val132 $ fromIntegral val232

idiv8 :: DivFunc8
idiv8 flags val1 val2 = (flags, result)
    where
        --todo: if val2 == 0 then DE
        opMod = mod :: Int16 -> Int16 -> Int16
        opDiv = div :: Int16 -> Int16 -> Int16
        val216 = signExtendWord val2 :: Uint16
        resultL = signedOp opDiv val1 val216
        resultH = signedOp opMod val1 val216
        result = (resultL .&. 0x00FF) .|. (shiftL resultH 8)

idiv16 :: DivFunc16
idiv16 flags val1H val1L val2 = (flags, resultH, resultL)
    where
        --todo: if val2 == 0 then DE
        opMod = mod :: Int32 -> Int32 -> Int32
        opDiv = div :: Int32 -> Int32 -> Int32
        val132 = ((shiftL (fromIntegral val1H) 16) .|. (fromIntegral val1L) :: Uint32)
        val232 = signExtendDoubleword32 val2
        resultL = fromIntegral $ signedOp opDiv val132 val232
        resultH = fromIntegral $ signedOp opMod val132 val232

-------------------------------------------------------------------------------

muldivInstr8 :: MuldivFunc8 -> FuncNV1M Uint8
muldivInstr8 func val = do
    alVal <- readOp al
    flags <- getFlags
    let (newFlags, axVal) = func flags alVal val
    writeOp ax axVal
    setFlags newFlags

muldivInstr16 :: MuldivFunc16 -> FuncNV1M Uint16
muldivInstr16 func val = do
    axVal <- readOp ax
    flags <- getFlags
    let (newFlags, dxValNew, axValNew) = func flags axVal val
    writeOp ax axValNew
    writeOp dx dxValNew
    setFlags newFlags

divInstr8 :: DivFunc8 -> FuncNV1M Uint8
divInstr8 func val = do
    axVal <- readOp ax
    flags <- getFlags
    let (newFlags, axValNew) = func flags axVal val
    writeOp ax axValNew
    setFlags newFlags

divInstr16 :: DivFunc16 -> FuncNV1M Uint16
divInstr16 func val = do
    axVal <- readOp ax
    dxVal <- readOp dx
    flags <- getFlags
    let (newFlags, dxValNew, axValNew) = func flags dxVal axVal val
    writeOp ax axValNew
    writeOp dx dxValNew
    setFlags newFlags
-}
-------------------------------------------------------------------------------
