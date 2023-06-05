module Prism.Instructions.Logical where

import Data.Bits
import Data.Int (Int8, Int16)

import Prism.Cpu
import Prism.InstructionM

-------------------------------------------------------------------------------

type RotateFunc v = Flags -> v -> Uint8 -> (Flags, v)

signRetained :: (OperandVal a) => a -> a -> Bool
signRetained before after = testBit before bit == testBit after bit
    where
        bit = (finiteBitSize before) - 1

lastBitNumShiftedL :: (OperandVal a) => a -> Int -> Int
lastBitNumShiftedL val shVal = (finiteBitSize val) - shVal

lastBitNumShiftedR :: (OperandVal a) => a -> Int -> Int
lastBitNumShiftedR _ shVal | shVal == 0 = 0
lastBitNumShiftedR _ shVal = shVal - 1

--Get last bit shifted/rotated left
lastBitShiftedL :: (OperandVal a) => a -> Int -> Bool
lastBitShiftedL val shVal = testBit val $ lastBitNumShiftedL val shVal

--Get last bit shifted/rotated right
lastBitShiftedR :: (OperandVal a) => a -> Int -> Bool
lastBitShiftedR val shVal = testBit val $ lastBitNumShiftedR val shVal

rotateFlagsOF :: (OperandVal a, OperandVal b) => Bool -> a -> a -> b -> Bool
rotateFlagsOF of_ _ _ shVal | shVal > 1 = of_
rotateFlagsOF _ val result _ = not $ signRetained val result

writeBit :: (OperandVal a) => a -> Int -> Bool -> a
writeBit val num True = setBit val num
writeBit val num False = clearBit val num

rotateFuncCl :: (CpuMonad m, OperandVal v) =>
    RotateFunc v -> v -> m v
rotateFuncCl func val = do
    shVal <- readOp cl
    flags <- getFlags
    let (flags_, val_) = func flags val shVal
    setFlags flags_
    return val_

rotateFuncOne :: (OperandVal v) => RotateFunc v -> FuncVF1 v
rotateFuncOne func flags val = func flags val 1

-------------------------------------------------------------------------------

notI :: (OperandVal v) => FuncV1 v
notI val = complement val

{-# SPECIALISE notI :: FuncV1 Uint8 #-}
{-# SPECIALISE notI :: FuncV1 Uint16 #-}

-------------------------------------------------------------------------------

logicalFunc :: (OperandVal v) => FuncVF1 v
logicalFunc flags val = (flags_, val)
    where
        flags_ = Flags False
                       (calcPF val)
                       False
                       (calcZF val)
                       (calcSF val)
                       False

andI :: (OperandVal v) => FuncVF2 v
andI flags source dest = logicalFunc flags $ source .&. dest

orI :: (OperandVal v) => FuncVF2 v
orI flags source dest = logicalFunc flags $ source .|. dest

xorI :: (OperandVal v) => FuncVF2 v
xorI flags source dest = logicalFunc flags $ xor source dest

testI :: (OperandVal v) => FuncVF2 v
testI flags source dest = logicalFunc flags $ source .&. dest

-------------------------------------------------------------------------------

shl :: (OperandVal v) => RotateFunc v
shl flags val shVal = (flags_, result)
    where
        result = shiftL val $ fromIntegral shVal
        cf_ = lastBitShiftedL val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF flags) val result shVal
        flags_ = Flags cf_
                       (calcPF result) 
                       (flagAF flags)
                       (calcZF result)
                       (calcSF result)
                       of_

shr :: (OperandVal v) => RotateFunc v
shr flags val shVal = (flags_, result)
    where
        result = shiftR val $ fromIntegral shVal
        cf_ = lastBitShiftedR val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF flags) val result shVal
        flags_ = Flags cf_
                       (calcPF result)
                       (flagAF flags)
                       (calcZF result)
                       (calcSF result)
                       of_

sar :: (OperandVal a, OperandVal b) => (Uint8 -> a -> a) -> RotateFunc b
sar op flags val shVal = (flags_, result)
    where
        result = signedOpS (op shVal) val
        cf_ = lastBitShiftedR val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF flags) val result shVal
        flags_ = Flags cf_
                       (calcPF result)
                       (flagAF flags)
                       (calcZF result)
                       (calcSF result)
                       of_

sar8 :: RotateFunc Uint8
sar8 flags val shVal = sar op1 flags val shVal
    where
        op1 sh1 = (flip shiftR $ fromIntegral sh1) :: Int8 -> Int8

sar16 :: RotateFunc Uint16
sar16 flags val shVal = sar op1 flags val shVal
    where
        op1 sh1 = (flip shiftR $ fromIntegral sh1) :: Int16 -> Int16

-------------------------------------------------------------------------------

rol :: (OperandVal v) => RotateFunc v
rol flags val rotVal = (flags_, result)
    where
        result = rotateL val $ fromIntegral rotVal
        cf_ = lastBitShiftedL val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF flags) val result rotVal
        flags_ = flags { flagCF = cf_, flagOF = of_ }

ror :: (OperandVal v) => RotateFunc v
ror flags val rotVal = (flags_, result)
    where
        result = rotateR val $ fromIntegral rotVal
        cf_ = lastBitShiftedR val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF flags) val result rotVal
        flags_ = flags { flagCF = cf_, flagOF = of_ }

rcl :: (OperandVal v) => RotateFunc v
rcl flags val rotVal = (flags_, result)
    where
        bitSize = fromIntegral (finiteBitSize val)
        lastBit = lastBitNumShiftedR val $ fromIntegral rotVal
        part1 = shiftL val $ fromIntegral rotVal
        part2 = shiftR val $ fromIntegral (bitSize - rotVal + 1)
        result = writeBit (part1 .|. part2) lastBit (flagCF flags)
        cf_ = lastBitShiftedL val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF flags) val result rotVal
        flags_ = flags { flagCF = cf_, flagOF = of_ }

rcr :: (OperandVal v) => RotateFunc v
rcr flags val rotVal = (flags_, result)
    where
        bitSize = fromIntegral (finiteBitSize val)
        lastBit = lastBitNumShiftedL val $ fromIntegral rotVal
        part1 = shiftR val $ fromIntegral rotVal
        part2 = shiftL val $ fromIntegral (bitSize - rotVal + 1)
        result = writeBit (part1 .|. part2) lastBit (flagCF flags)
        cf_ = lastBitShiftedR val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF flags) val result rotVal
        flags_ = flags { flagCF = cf_, flagOF = of_ }

-------------------------------------------------------------------------------
