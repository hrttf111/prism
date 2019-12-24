module Instruction.Logical where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Bits
import Data.Int

import Prism
import PrismDecoder
import PrismCpu

-------------------------------------------------------------------------------

type RotateFunc b = Ctx -> b -> Uint8 -> (Ctx, b)

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

writeBit :: OperandVal a => a -> Int -> Bool -> a
writeBit val num True = setBit val num
writeBit val num False = clearBit val num

rotateFuncCl :: OperandVal b =>
    RotateFunc b -> Ctx -> b -> PrismCtx IO (Ctx, b)
rotateFuncCl func ctx val = do
    shVal <- readOp ctx cl
    return $ func ctx val shVal

rotateFuncOne :: OperandVal b => RotateFunc b -> FuncV1 b
rotateFuncOne func ctx val = func ctx val 1

-------------------------------------------------------------------------------

notI :: OperandVal b => FuncV1 b
notI ctx val = (ctx, complement val)

{-# SPECIALISE notI :: FuncV1 Uint8 #-}
{-# SPECIALISE notI :: FuncV1 Uint16 #-}

-------------------------------------------------------------------------------

logicalFunc :: OperandVal b => FuncV1 b
logicalFunc ctx val = (newCtx, val)
    where
        flags = Flags False (calcPF val) (flagAF . ctxFlags $ ctx) (calcZF val) (calcSF val) False
        newCtx = ctx { ctxFlags = flags }

andI :: OperandVal b => FuncV2 b
andI ctx source dest = logicalFunc ctx $ source .&. dest

orI :: OperandVal b => FuncV2 b
orI ctx source dest = logicalFunc ctx $ source .|. dest

xorI :: OperandVal b => FuncV2 b
xorI ctx source dest = logicalFunc ctx $ xor source dest

testI :: OperandVal b => FuncV2 b
testI ctx source dest = logicalFunc ctx $ source .&. dest

-------------------------------------------------------------------------------

shl :: OperandVal b => RotateFunc b
shl ctx val shVal = (newCtx, result)
    where
        result = shiftL val $ fromIntegral shVal
        cf_ = lastBitShiftedL val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result shVal
        flags = Flags cf_ (calcPF result) (flagAF . ctxFlags $ ctx) (calcZF result) (calcSF result) of_
        newCtx = ctx { ctxFlags = flags }

shr :: OperandVal b => RotateFunc b
shr ctx val shVal = (newCtx, result)
    where
        result = shiftR val $ fromIntegral shVal
        cf_ = lastBitShiftedR val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result shVal
        flags = Flags cf_ (calcPF result) (flagAF . ctxFlags $ ctx) (calcZF result) (calcSF result) of_
        newCtx = ctx { ctxFlags = flags }

sar :: (OperandVal a, OperandVal b) => (Uint8 -> a -> a) -> RotateFunc b
sar op ctx val shVal = (newCtx, result)
    where
        result = signedOpS (op shVal) val
        cf_ = lastBitShiftedR val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result shVal
        flags = Flags cf_ (calcPF result) (flagAF . ctxFlags $ ctx) (calcZF result) (calcSF result) of_
        newCtx = ctx { ctxFlags = flags }

sar8 :: RotateFunc Uint8
sar8 ctx val shVal = sar op1 ctx val shVal
    where
        op1 sh1 = (flip shiftR $ fromIntegral sh1) :: Int8 -> Int8

sar16 :: RotateFunc Uint16
sar16 ctx val shVal = sar op1 ctx val shVal
    where
        op1 sh1 = (flip shiftR $ fromIntegral sh1) :: Int16 -> Int16

-------------------------------------------------------------------------------

rol :: OperandVal b => RotateFunc b
rol ctx val rotVal = (newCtx, result)
    where
        result = rotateL val $ fromIntegral rotVal
        cf_ = lastBitShiftedL val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }

ror :: OperandVal b => RotateFunc b
ror ctx val rotVal = (newCtx, result)
    where
        result = rotateR val $ fromIntegral rotVal
        cf_ = lastBitShiftedR val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }

rcl :: OperandVal b => RotateFunc b
rcl ctx val rotVal = (newCtx, result)
    where
        bitSize = fromIntegral (finiteBitSize val)
        lastBit = lastBitNumShiftedR val $ fromIntegral rotVal
        part1 = shiftL val $ fromIntegral rotVal
        part2 = shiftR val $ fromIntegral (bitSize - rotVal + 1)
        result = writeBit (part1 .|. part2) lastBit (flagCF . ctxFlags $ ctx)
        cf_ = lastBitShiftedL val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }

rcr :: OperandVal b => RotateFunc b
rcr ctx val rotVal = (newCtx, result)
    where
        bitSize = fromIntegral (finiteBitSize val)
        lastBit = lastBitNumShiftedL val $ fromIntegral rotVal
        part1 = shiftR val $ fromIntegral rotVal
        part2 = shiftL val $ fromIntegral (bitSize - rotVal + 1)
        result = writeBit (part1 .|. part2) lastBit (flagCF . ctxFlags $ ctx)
        cf_ = lastBitShiftedR val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

logicalInstrList = [
        --OR
        makeInstructionS 0x08 Nothing (decodeRM8 (instrRegToRm orI) (instrRegToRm orI)),
        makeInstructionS 0x09 Nothing (decodeRM16 (instrRegToRm orI) (instrRegToRm orI)),
        makeInstructionS 0x0A Nothing (decodeRM8 (instrRmToReg orI) (instrRmToReg orI)),
        makeInstructionS 0x0B Nothing (decodeRM16 (instrRmToReg orI) (instrRmToReg orI)),
        makeInstructionS 0x0C Nothing (decodeStRI al $ instrOI1 orI),
        makeInstructionS 0x0D Nothing (decodeStRI ax $ instrOI1 orI),
        makeInstructionS 0x80 (Just 1) (decodeNI8 (instrOI1 orI) (instrOI1 orI)),
        makeInstructionS 0x81 (Just 1) (decodeNI16 (instrOI1 orI) (instrOI1 orI)),
        makeInstructionS 0x82 (Just 1) (decodeNI8 (instrOI1 orI) (instrOI1 orI)),
        --makeInstructionS 0x83 (Just 1) (decodeN16Imm8 (instrRegImm16 orI) (instrMemImm16 orI)),
        makeInstructionS 0x83 (Just 1) (decodeNC16 (instrOI1 orI) (instrOI1 orI)),
        --AND
        makeInstructionS 0x20 Nothing (decodeRM8 (instrRegToRm andI) (instrRegToRm andI)),
        makeInstructionS 0x21 Nothing (decodeRM16 (instrRegToRm andI) (instrRegToRm andI)),
        makeInstructionS 0x22 Nothing (decodeRM8 (instrRmToReg andI) (instrRmToReg andI)),
        makeInstructionS 0x23 Nothing (decodeRM16 (instrRmToReg andI) (instrRmToReg andI)),
        makeInstructionS 0x24 Nothing (decodeStRI al $ instrOI1 andI),
        makeInstructionS 0x25 Nothing (decodeStRI ax $ instrOI1 andI),
        makeInstructionS 0x80 (Just 4) (decodeNI8 (instrOI1 andI) (instrOI1 andI)),
        makeInstructionS 0x81 (Just 4) (decodeNI16 (instrOI1 andI) (instrOI1 andI)),
        makeInstructionS 0x82 (Just 4) (decodeNI8 (instrOI1 andI) (instrOI1 andI)),
        --makeInstructionS 0x83 (Just 4) (decodeN16Imm8 (instrRegImm16 andI) (instrMemImm16 andI)),
        makeInstructionS 0x83 (Just 4) (decodeNC16 (instrOI1 andI) (instrOI1 andI)),
        --XOR
        makeInstructionS 0x30 Nothing (decodeRM8 (instrRegToRm xorI) (instrRegToRm xorI)),
        makeInstructionS 0x31 Nothing (decodeRM16 (instrRegToRm xorI) (instrRegToRm xorI)),
        makeInstructionS 0x32 Nothing (decodeRM8 (instrRmToReg xorI) (instrRmToReg xorI)),
        makeInstructionS 0x33 Nothing (decodeRM16 (instrRmToReg xorI) (instrRmToReg xorI)),
        makeInstructionS 0x34 Nothing (decodeStRI al $ instrOI1 xorI),
        makeInstructionS 0x35 Nothing (decodeStRI ax $ instrOI1 xorI),
        makeInstructionS 0x80 (Just 6) (decodeNI8 (instrOI1 xorI) (instrOI1 xorI)),
        makeInstructionS 0x81 (Just 6) (decodeNI16 (instrOI1 xorI) (instrOI1 xorI)),
        makeInstructionS 0x82 (Just 6) (decodeNI8 (instrOI1 xorI) (instrOI1 xorI)),
        --makeInstructionS 0x83 (Just 6) (decodeN16Imm8 (instrRegImm16 xorI) (instrMemImm16 xorI)),
        makeInstructionS 0x83 (Just 6) (decodeNC16 (instrOI1 xorI) (instrOI1 xorI)),
        --TEST
        makeInstructionS 0x84 Nothing (decodeRM8 (instrRegToRm testI) (instrRegToRm testI)),
        makeInstructionS 0x85 Nothing (decodeRM16 (instrRegToRm testI) (instrRegToRm testI)),
        makeInstructionS 0xA8 Nothing (decodeStRI al $ instrOI1 testI),
        makeInstructionS 0xA9 Nothing (decodeStRI ax $ instrOI1 testI),
        makeInstructionS 0xF6 (Just 0) (decodeNI8 (instrOI1 testI) (instrOI1 testI)),
        makeInstructionS 0xF7 (Just 0) (decodeNI16 (instrOI1 testI) (instrOI1 testI)),
        --SHL/SAL
        makeInstructionS 0xD0 (Just 4) (decodeN8 (instrO1 $ rotateFuncOne shl) (instrO1 $ rotateFuncOne shl)),
        makeInstructionS 0xD1 (Just 4) (decodeN16 (instrO1 $ rotateFuncOne shl) (instrO1 $ rotateFuncOne shl)),
        makeInstructionS 0xD2 (Just 4) (decodeN8 (instrOV1 $ rotateFuncCl shl) (instrOV1 $ rotateFuncCl shl)),
        makeInstructionS 0xD3 (Just 4) (decodeN16 (instrOV1 $ rotateFuncCl shl) (instrOV1 $ rotateFuncCl shl)),
        --SHR
        makeInstructionS 0xD0 (Just 5) (decodeN8 (instrO1 $ rotateFuncOne shr) (instrO1 $ rotateFuncOne shr)),
        makeInstructionS 0xD1 (Just 5) (decodeN16 (instrO1 $ rotateFuncOne shr) (instrO1 $ rotateFuncOne shr)),
        makeInstructionS 0xD2 (Just 5) (decodeN8 (instrOV1 $ rotateFuncCl shr) (instrOV1 $ rotateFuncCl shr)),
        makeInstructionS 0xD3 (Just 5) (decodeN16 (instrOV1 $ rotateFuncCl shr) (instrOV1 $ rotateFuncCl shr)),
        --SAR
        makeInstructionS 0xD0 (Just 7) (decodeN8 (instrO1 $ rotateFuncOne sar8) (instrO1 $ rotateFuncOne sar8)),
        makeInstructionS 0xD1 (Just 7) (decodeN16 (instrO1 $ rotateFuncOne sar16) (instrO1 $ rotateFuncOne sar16)),
        makeInstructionS 0xD2 (Just 7) (decodeN8 (instrOV1 $ rotateFuncCl sar8) (instrOV1 $ rotateFuncCl sar8)),
        makeInstructionS 0xD3 (Just 7) (decodeN16 (instrOV1 $ rotateFuncCl sar16) (instrOV1 $ rotateFuncCl sar16)),
        --ROL
        makeInstructionS 0xD0 (Just 0) (decodeN8 (instrO1 $ rotateFuncOne rol) (instrO1 $ rotateFuncOne rol)),
        makeInstructionS 0xD1 (Just 0) (decodeN16 (instrO1 $ rotateFuncOne rol) (instrO1 $ rotateFuncOne rol)),
        makeInstructionS 0xD2 (Just 0) (decodeN8 (instrOV1 $ rotateFuncCl rol) (instrOV1 $ rotateFuncCl rol)),
        makeInstructionS 0xD3 (Just 0) (decodeN16 (instrOV1 $ rotateFuncCl rol) (instrOV1 $ rotateFuncCl rol)),
        --ROR
        makeInstructionS 0xD0 (Just 1) (decodeN8 (instrO1 $ rotateFuncOne ror) (instrO1 $ rotateFuncOne ror)),
        makeInstructionS 0xD1 (Just 1) (decodeN16 (instrO1 $ rotateFuncOne ror) (instrO1 $ rotateFuncOne ror)),
        makeInstructionS 0xD2 (Just 1) (decodeN8 (instrOV1 $ rotateFuncCl ror) (instrOV1 $ rotateFuncCl ror)),
        makeInstructionS 0xD3 (Just 1) (decodeN16 (instrOV1 $ rotateFuncCl ror) (instrOV1 $ rotateFuncCl ror)),
        --RCL
        makeInstructionS 0xD0 (Just 2) (decodeN8 (instrO1 $ rotateFuncOne rcl) (instrO1 $ rotateFuncOne rcl)),
        makeInstructionS 0xD1 (Just 2) (decodeN16 (instrO1 $ rotateFuncOne rcl) (instrO1 $ rotateFuncOne rcl)),
        makeInstructionS 0xD2 (Just 2) (decodeN8 (instrOV1 $ rotateFuncCl rcl) (instrOV1 $ rotateFuncCl rcl)),
        makeInstructionS 0xD3 (Just 2) (decodeN16 (instrOV1 $ rotateFuncCl rcl) (instrOV1 $ rotateFuncCl rcl)),
        --RCR
        makeInstructionS 0xD0 (Just 3) (decodeN8 (instrO1 $ rotateFuncOne rcr) (instrO1 $ rotateFuncOne rcr)),
        makeInstructionS 0xD1 (Just 3) (decodeN16 (instrO1 $ rotateFuncOne rcr) (instrO1 $ rotateFuncOne rcr)),
        makeInstructionS 0xD2 (Just 3) (decodeN8 (instrOV1 $ rotateFuncCl rcr) (instrOV1 $ rotateFuncCl rcr)),
        makeInstructionS 0xD3 (Just 3) (decodeN16 (instrOV1 $ rotateFuncCl rcr) (instrOV1 $ rotateFuncCl rcr)),
        --NOT
        makeInstructionS 0xF6 (Just 2) (decodeN8 (instrO1 notI) (instrO1 notI)),
        makeInstructionS 0xF7 (Just 2) (decodeN16 (instrO1 notI) (instrO1 notI))
    ]
