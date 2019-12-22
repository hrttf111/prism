module Instruction.Logical where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Bits
import Data.Int

import Prism
import PrismDecoder
import PrismCpu

-------------------------------------------------------------------------------

type RotateFunc b = Ctx -> b -> Uint8 -> (Ctx, b)
type RotateFunc8 = Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
type RotateFunc16 = Ctx -> Uint16 -> Uint8 -> (Ctx, Uint16)

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

sar8 :: RotateFunc8
sar8 ctx val shVal = sar op1 ctx val shVal
    where
        op1 sh1 = (flip shiftR $ fromIntegral sh1) :: Int8 -> Int8

sar16 :: RotateFunc16
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
        makeInstructionS 0x08 Nothing (decodeRm8 (instrRegToReg8RegToRm orI) (instrRegToMem8 orI)),
        makeInstructionS 0x09 Nothing (decodeRm16 (instrRegToReg16RegToRm orI) (instrRegToMem16 orI)),
        makeInstructionS 0x0A Nothing (decodeRm8 (instrRegToReg8RmToReg orI) (instrMemToReg8 orI)),
        makeInstructionS 0x0B Nothing (decodeRm16 (instrRegToReg16RmToReg orI) (instrMemToReg16 orI)),
        makeInstructionS 0x0C Nothing (decodeAcc8 al $ instrRegImm8 orI),
        makeInstructionS 0x0D Nothing (decodeAcc16 ax $ instrRegImm16 orI),
        makeInstructionS 0x80 (Just 1) (decodeN8Imm8 (instrRegImm8 orI) (instrMemImm8 orI)),
        makeInstructionS 0x81 (Just 1) (decodeN16Imm (instrRegImm16 orI) (instrMemImm16 orI)),
        makeInstructionS 0x82 (Just 1) (decodeN8Imm8 (instrRegImm8 orI) (instrMemImm8 orI)),
        makeInstructionS 0x83 (Just 1) (decodeN16Imm8 (instrRegImm16 orI) (instrMemImm16 orI)),
        --AND
        makeInstructionS 0x20 Nothing (decodeRm8 (instrRegToReg8RegToRm andI) (instrRegToMem8 andI)),
        makeInstructionS 0x21 Nothing (decodeRm16 (instrRegToReg16RegToRm andI) (instrRegToMem16 andI)),
        makeInstructionS 0x22 Nothing (decodeRm8 (instrRegToReg8RmToReg andI) (instrMemToReg8 andI)),
        makeInstructionS 0x23 Nothing (decodeRm16 (instrRegToReg16RmToReg andI) (instrMemToReg16 andI)),
        makeInstructionS 0x24 Nothing (decodeAcc8 al $ instrRegImm8 andI),
        makeInstructionS 0x25 Nothing (decodeAcc16 ax $ instrRegImm16 andI),
        makeInstructionS 0x80 (Just 4) (decodeN8Imm8 (instrRegImm8 andI) (instrMemImm8 andI)),
        makeInstructionS 0x81 (Just 4) (decodeN16Imm (instrRegImm16 andI) (instrMemImm16 andI)),
        makeInstructionS 0x82 (Just 4) (decodeN8Imm8 (instrRegImm8 andI) (instrMemImm8 andI)),
        makeInstructionS 0x83 (Just 4) (decodeN16Imm8 (instrRegImm16 andI) (instrMemImm16 andI)),
        --XOR
        makeInstructionS 0x30 Nothing (decodeRm8 (instrRegToReg8RegToRm xorI) (instrRegToMem8 xorI)),
        makeInstructionS 0x31 Nothing (decodeRm16 (instrRegToReg16RegToRm xorI) (instrRegToMem16 xorI)),
        makeInstructionS 0x32 Nothing (decodeRm8 (instrRegToReg8RmToReg xorI) (instrMemToReg8 xorI)),
        makeInstructionS 0x33 Nothing (decodeRm16 (instrRegToReg16RmToReg xorI) (instrMemToReg16 xorI)),
        makeInstructionS 0x34 Nothing (decodeAcc8 al $ instrRegImm8 xorI),
        makeInstructionS 0x35 Nothing (decodeAcc16 ax $ instrRegImm16 xorI),
        makeInstructionS 0x80 (Just 6) (decodeN8Imm8 (instrRegImm8 xorI) (instrMemImm8 xorI)),
        makeInstructionS 0x81 (Just 6) (decodeN16Imm (instrRegImm16 xorI) (instrMemImm16 xorI)),
        makeInstructionS 0x82 (Just 6) (decodeN8Imm8 (instrRegImm8 xorI) (instrMemImm8 xorI)),
        makeInstructionS 0x83 (Just 6) (decodeN16Imm8 (instrRegImm16 xorI) (instrMemImm16 xorI)),
        --TEST
        makeInstructionS 0x84 Nothing (decodeRm8 (instrRegToReg8RegToRm testI) (instrRegToMem8 testI)),
        makeInstructionS 0x85 Nothing (decodeRm16 (instrRegToReg16RegToRm testI) (instrRegToMem16 testI)),
        makeInstructionS 0xA8 Nothing (decodeAcc8 al $ instrRegImm8 testI),
        makeInstructionS 0xA9 Nothing (decodeAcc16 ax $ instrRegImm16 testI),
        makeInstructionS 0xF6 (Just 0) (decodeN8Imm8 (instrRegImm8 testI) (instrMemImm8 testI)),
        makeInstructionS 0xF7 (Just 0) (decodeN16Imm (instrRegImm16 testI) (instrMemImm16 testI)),
        --SHL/SAL
        makeInstructionS 0xD0 (Just 4) (decodeN8 (instrReg8 $ rotateFuncOne shl) (instrMem8 $ rotateFuncOne shl)),
        makeInstructionS 0xD1 (Just 4) (decodeN16 (instrReg16 $ rotateFuncOne shl) (instrMem16 $ rotateFuncOne shl)),
        makeInstructionS 0xD2 (Just 4) (decodeN8 (instrRegVal8 $ rotateFuncCl shl) (instrMemVal8 $ rotateFuncCl shl)),
        makeInstructionS 0xD3 (Just 4) (decodeN16 (instrRegVal16 $ rotateFuncCl shl) (instrMemVal16 $ rotateFuncCl shl)),
        --SHR
        makeInstructionS 0xD0 (Just 5) (decodeN8 (instrReg8 $ rotateFuncOne shr) (instrMem8 $ rotateFuncOne shr)),
        makeInstructionS 0xD1 (Just 5) (decodeN16 (instrReg16 $ rotateFuncOne shr) (instrMem16 $ rotateFuncOne shr)),
        makeInstructionS 0xD2 (Just 5) (decodeN8 (instrRegVal8 $ rotateFuncCl shr) (instrMemVal8 $ rotateFuncCl shr)),
        makeInstructionS 0xD3 (Just 5) (decodeN16 (instrRegVal16 $ rotateFuncCl shr) (instrMemVal16 $ rotateFuncCl shr)),
        --SAR
        makeInstructionS 0xD0 (Just 7) (decodeN8 (instrReg8 $ rotateFuncOne sar8) (instrMem8 $ rotateFuncOne sar8)),
        makeInstructionS 0xD1 (Just 7) (decodeN16 (instrReg16 $ rotateFuncOne sar16) (instrMem16 $ rotateFuncOne sar16)),
        makeInstructionS 0xD2 (Just 7) (decodeN8 (instrRegVal8 $ rotateFuncCl sar8) (instrMemVal8 $ rotateFuncCl sar8)),
        makeInstructionS 0xD3 (Just 7) (decodeN16 (instrRegVal16 $ rotateFuncCl sar16) (instrMemVal16 $ rotateFuncCl sar16)),
        --ROL
        makeInstructionS 0xD0 (Just 0) (decodeN8 (instrReg8 $ rotateFuncOne rol) (instrMem8 $ rotateFuncOne rol)),
        makeInstructionS 0xD1 (Just 0) (decodeN16 (instrReg16 $ rotateFuncOne rol) (instrMem16 $ rotateFuncOne rol)),
        makeInstructionS 0xD2 (Just 0) (decodeN8 (instrRegVal8 $ rotateFuncCl rol) (instrMemVal8 $ rotateFuncCl rol)),
        makeInstructionS 0xD3 (Just 0) (decodeN16 (instrRegVal16 $ rotateFuncCl rol) (instrMemVal16 $ rotateFuncCl rol)),
        --ROR
        makeInstructionS 0xD0 (Just 1) (decodeN8 (instrReg8 $ rotateFuncOne ror) (instrMem8 $ rotateFuncOne ror)),
        makeInstructionS 0xD1 (Just 1) (decodeN16 (instrReg16 $ rotateFuncOne ror) (instrMem16 $ rotateFuncOne ror)),
        makeInstructionS 0xD2 (Just 1) (decodeN8 (instrRegVal8 $ rotateFuncCl ror) (instrMemVal8 $ rotateFuncCl ror)),
        makeInstructionS 0xD3 (Just 1) (decodeN16 (instrRegVal16 $ rotateFuncCl ror) (instrMemVal16 $ rotateFuncCl ror)),
        --RCL
        makeInstructionS 0xD0 (Just 2) (decodeN8 (instrReg8 $ rotateFuncOne rcl) (instrMem8 $ rotateFuncOne rcl)),
        makeInstructionS 0xD1 (Just 2) (decodeN16 (instrReg16 $ rotateFuncOne rcl) (instrMem16 $ rotateFuncOne rcl)),
        makeInstructionS 0xD2 (Just 2) (decodeN8 (instrRegVal8 $ rotateFuncCl rcl) (instrMemVal8 $ rotateFuncCl rcl)),
        makeInstructionS 0xD3 (Just 2) (decodeN16 (instrRegVal16 $ rotateFuncCl rcl) (instrMemVal16 $ rotateFuncCl rcl)),
        --RCR
        makeInstructionS 0xD0 (Just 3) (decodeN8 (instrReg8 $ rotateFuncOne rcr) (instrMem8 $ rotateFuncOne rcr)),
        makeInstructionS 0xD1 (Just 3) (decodeN16 (instrReg16 $ rotateFuncOne rcr) (instrMem16 $ rotateFuncOne rcr)),
        makeInstructionS 0xD2 (Just 3) (decodeN8 (instrRegVal8 $ rotateFuncCl rcr) (instrMemVal8 $ rotateFuncCl rcr)),
        makeInstructionS 0xD3 (Just 3) (decodeN16 (instrRegVal16 $ rotateFuncCl rcr) (instrMemVal16 $ rotateFuncCl rcr)),
        --NOT
        makeInstructionS 0xF6 (Just 2) (decodeN8 (instrReg8 notI) (instrMem8 notI)),
        makeInstructionS 0xF7 (Just 2) (decodeN16 (instrReg16 notI) (instrMem16 notI))
    ]
