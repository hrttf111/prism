module Instruction.Logical where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Bits

import Prism
import PrismDecoder
import PrismCpu

-------------------------------------------------------------------------------

not8 :: Ctx -> Uint8 -> (Ctx, Uint8)
not8 ctx val = (ctx, complement val)

not16 :: Ctx -> Uint16 -> (Ctx, Uint16)
not16 ctx val = (ctx, complement val)

-------------------------------------------------------------------------------

logicalUpdateFlags8 :: Ctx -> Uint8 -> Ctx
logicalUpdateFlags8 ctx val = newCtx
    where
        flags = Flags False (calcPF8 val) (flagAF . ctxFlags $ ctx) (calcZF8 val) (calcSF8 val) False
        newCtx = ctx { ctxFlags = flags }

logicalUpdateFlags16 :: Ctx -> Uint16 -> Ctx
logicalUpdateFlags16 ctx val = newCtx
    where
        flags = Flags False (calcPF16 val) (flagAF . ctxFlags $ ctx) (calcZF16 val) (calcSF16 val) False
        newCtx = ctx { ctxFlags = flags }

and8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
and8 ctx source dest = (logicalUpdateFlags8 ctx result, result)
    where
        result = source .&. dest

and16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)
and16 ctx source dest = (logicalUpdateFlags16 ctx result, result)
    where
        result = source .&. dest

or8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
or8 ctx source dest = (logicalUpdateFlags8 ctx result, result)
    where
        result = source .|. dest

or16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)
or16 ctx source dest = (logicalUpdateFlags16 ctx result, result)
    where
        result = source .|. dest

xor8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
xor8 ctx source dest = (logicalUpdateFlags8 ctx result, result)
    where
        result = xor source dest

xor16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)
xor16 ctx source dest = (logicalUpdateFlags16 ctx result, result)
    where
        result = xor source dest

test8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
test8 ctx source dest = (logicalUpdateFlags8 ctx result, dest)
    where
        result = source .&. dest

test16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)
test16 ctx source dest = (logicalUpdateFlags16 ctx result, dest)
    where
        result = source .&. dest

-------------------------------------------------------------------------------

signRetained :: (Bits a, FiniteBits a) => a -> a -> Bool
signRetained before after = testBit before bit == testBit after bit
    where
        bit = (finiteBitSize before) - 1

lastBitNumShiftedL :: (Bits a, FiniteBits a) => a -> Int -> Int
lastBitNumShiftedL val shVal = (finiteBitSize val) - shVal

lastBitNumShiftedR :: (Bits a, FiniteBits a) => a -> Int -> Int
lastBitNumShiftedR _ shVal | shVal == 0 = 0
lastBitNumShiftedR _ shVal = shVal - 1

--Get last bit shifted/rotated left
lastBitShiftedL :: (Bits a, FiniteBits a) => a -> Int -> Bool
lastBitShiftedL val shVal = testBit val $ lastBitNumShiftedL val shVal

--Get last bit shifted/rotated right
lastBitShiftedR :: (Bits a, FiniteBits a) => a -> Int -> Bool
lastBitShiftedR val shVal = testBit val $ lastBitNumShiftedR val shVal

rotateFlagsOF :: (Bits a, FiniteBits a, Integral b, Num b, Ord b) => Bool -> a -> a -> b -> Bool
rotateFlagsOF of_ _ _ shVal | shVal > 1 = of_
rotateFlagsOF _ val result _ = not $ signRetained val result

--todo: specialize

type RotateFunc8 = Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
type RotateFunc16 = Ctx -> Uint16 -> Uint8 -> (Ctx, Uint16)

shl8 :: RotateFunc8
shl8 ctx val shVal = (newCtx, result)
    where
        result = shiftL val $ fromIntegral shVal
        cf_ = lastBitShiftedL val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result shVal
        flags = Flags cf_ (calcPF8 result) (flagAF . ctxFlags $ ctx) (calcZF8 result) (calcSF8 result) of_
        newCtx = ctx { ctxFlags = flags }

shl16 :: RotateFunc16
shl16 ctx val shVal = (newCtx, result)
    where
        result = shiftL val $ fromIntegral shVal
        cf_ = lastBitShiftedL val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result shVal
        flags = Flags cf_ (calcPF16 result) (flagAF . ctxFlags $ ctx) (calcZF16 result) (calcSF16 result) of_
        newCtx = ctx { ctxFlags = flags }

sal8 = shl8
sal16 = shl16

shr8 :: RotateFunc8
shr8 ctx val shVal = (newCtx, result)
    where
        result = shiftR val $ fromIntegral shVal
        cf_ = lastBitShiftedR val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result shVal
        flags = Flags cf_ (calcPF8 result) (flagAF . ctxFlags $ ctx) (calcZF8 result) (calcSF8 result) of_
        newCtx = ctx { ctxFlags = flags }

shr16 :: RotateFunc16
shr16 ctx val shVal = (newCtx, result)
    where
        result = shiftR val $ fromIntegral shVal
        cf_ = lastBitShiftedR val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result shVal
        flags = Flags cf_ (calcPF16 result) (flagAF . ctxFlags $ ctx) (calcZF16 result) (calcSF16 result) of_
        newCtx = ctx { ctxFlags = flags }

--todo: set 1`s for all rotated bits
sar8 :: RotateFunc8
sar8 ctx val shVal = (newCtx, result)
    where
        result = shiftR val $ fromIntegral shVal
        cf_ = lastBitShiftedR val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result shVal
        flags = Flags cf_ (calcPF8 result) (flagAF . ctxFlags $ ctx) (calcZF8 result) (calcSF8 result) of_
        newCtx = ctx { ctxFlags = flags }

sar16 :: RotateFunc16
sar16 ctx val shVal = (newCtx, result)
    where
        result = shiftR val $ fromIntegral shVal
        cf_ = lastBitShiftedR val $ fromIntegral shVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result shVal
        flags = Flags cf_ (calcPF16 result) (flagAF . ctxFlags $ ctx) (calcZF16 result) (calcSF16 result) of_
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

rotateFuncCl8 :: RotateFunc8 -> Ctx -> Uint8 -> PrismCtx IO (Ctx, Uint8)
rotateFuncCl8 func ctx val = do
    shVal <- readReg8 memReg cl
    return $ func ctx val shVal
    where
        memReg = ctxReg ctx

rotateFuncCl16 :: RotateFunc16 -> Ctx -> Uint16 -> PrismCtx IO (Ctx, Uint16)
rotateFuncCl16 func ctx val = do
    shVal <- readReg8 memReg cl
    return $ func ctx val shVal
    where
        memReg = ctxReg ctx

rotateFuncOne8 :: RotateFunc8 -> Func8
rotateFuncOne8 func ctx val = func ctx val 1

rotateFuncOne16 :: RotateFunc16 -> Func16
rotateFuncOne16 func ctx val = func ctx val 1

-------------------------------------------------------------------------------

rol8 :: RotateFunc8
rol8 ctx val rotVal = (newCtx, result)
    where
        result = rotateL val $ fromIntegral rotVal
        cf_ = lastBitShiftedL val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }
        
rol16 :: RotateFunc16
rol16 ctx val rotVal = (newCtx, result)
    where
        result = rotateL val $ fromIntegral rotVal
        cf_ = lastBitShiftedL val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }

ror8 :: RotateFunc8
ror8 ctx val rotVal = (newCtx, result)
    where
        result = rotateR val $ fromIntegral rotVal
        cf_ = lastBitShiftedR val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }

ror16 :: RotateFunc16
ror16 ctx val rotVal = (newCtx, result)
    where
        result = rotateR val $ fromIntegral rotVal
        cf_ = lastBitShiftedR val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }

writeBit :: (Bits a, FiniteBits a, Integral a, Num a, Ord a) => a -> Int -> Bool -> a
writeBit val num True = setBit val num
writeBit val num False = clearBit val num

rcl8 :: RotateFunc8
rcl8 ctx val rotVal = (newCtx, result)
    where
        lastBit = lastBitNumShiftedR val $ fromIntegral rotVal
        part1 = shiftL val $ fromIntegral rotVal
        part2 = shiftR val $ fromIntegral (8 - rotVal + 1)
        result = writeBit (part1 .|. part2) lastBit (flagCF . ctxFlags $ ctx)
        cf_ = lastBitShiftedL val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }

rcl16 :: RotateFunc16
rcl16 ctx val rotVal = (newCtx, result)
    where
        lastBit = lastBitNumShiftedR val $ fromIntegral rotVal
        part1 = shiftL val $ fromIntegral rotVal
        part2 = shiftR val $ fromIntegral (16 - rotVal + 1)
        result = writeBit (part1 .|. part2) lastBit (flagCF . ctxFlags $ ctx)
        cf_ = lastBitShiftedL val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }

rcr8 :: RotateFunc8
rcr8 ctx val rotVal = (newCtx, result)
    where
        lastBit = lastBitNumShiftedL val $ fromIntegral rotVal
        part1 = shiftR val $ fromIntegral rotVal
        part2 = shiftL val $ fromIntegral (8 - rotVal + 1)
        result = writeBit (part1 .|. part2) lastBit (flagCF . ctxFlags $ ctx)
        cf_ = lastBitShiftedR val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }

rcr16 :: RotateFunc16
rcr16 ctx val rotVal = (newCtx, result)
    where
        lastBit = lastBitNumShiftedL val $ fromIntegral rotVal
        part1 = shiftR val $ fromIntegral rotVal
        part2 = shiftL val $ fromIntegral (16 - rotVal + 1)
        result = writeBit (part1 .|. part2) lastBit (flagCF . ctxFlags $ ctx)
        cf_ = lastBitShiftedR val $ fromIntegral rotVal
        of_ = rotateFlagsOF (flagOF . ctxFlags $ ctx) val result rotVal
        flags = (ctxFlags ctx) { flagCF = cf_, flagOF = of_ }
        newCtx = ctx { ctxFlags = flags }

-------------------------------------------------------------------------------

logicalInstrList = [
        --OR
        makeInstructionS 0x08 Nothing (decodeRm8 (instrRegToReg8RegToRm or8) (instrRegToMem8 or8)),
        makeInstructionS 0x09 Nothing (decodeRm16 (instrRegToReg16RegToRm or16) (instrRegToMem16 or16)),
        makeInstructionS 0x0A Nothing (decodeRm8 (instrRegToReg8RmToReg or8) (instrMemToReg8 or8)),
        makeInstructionS 0x0B Nothing (decodeRm16 (instrRegToReg16RmToReg or16) (instrMemToReg16 or16)),
        makeInstructionS 0x0C Nothing (decodeAcc8 al $ instrRegImm8 or8),
        makeInstructionS 0x0D Nothing (decodeAcc16 ax $ instrRegImm16 or16),
        makeInstructionS 0x80 (Just 1) (decodeN8Imm8 (instrRegImm8 or8) (instrMemImm8 or8)),
        makeInstructionS 0x81 (Just 1) (decodeN16Imm (instrRegImm16 or16) (instrMemImm16 or16)),
        makeInstructionS 0x82 (Just 1) (decodeN8Imm8 (instrRegImm8 or8) (instrMemImm8 or8)),
        makeInstructionS 0x83 (Just 1) (decodeN16Imm8 (instrRegImm16 or16) (instrMemImm16 or16)),
        --AND
        makeInstructionS 0x20 Nothing (decodeRm8 (instrRegToReg8RegToRm and8) (instrRegToMem8 and8)),
        makeInstructionS 0x21 Nothing (decodeRm16 (instrRegToReg16RegToRm and16) (instrRegToMem16 and16)),
        makeInstructionS 0x22 Nothing (decodeRm8 (instrRegToReg8RmToReg and8) (instrMemToReg8 and8)),
        makeInstructionS 0x23 Nothing (decodeRm16 (instrRegToReg16RmToReg and16) (instrMemToReg16 and16)),
        makeInstructionS 0x24 Nothing (decodeAcc8 al $ instrRegImm8 and8),
        makeInstructionS 0x25 Nothing (decodeAcc16 ax $ instrRegImm16 and16),
        makeInstructionS 0x80 (Just 4) (decodeN8Imm8 (instrRegImm8 and8) (instrMemImm8 and8)),
        makeInstructionS 0x81 (Just 4) (decodeN16Imm (instrRegImm16 and16) (instrMemImm16 and16)),
        makeInstructionS 0x82 (Just 4) (decodeN8Imm8 (instrRegImm8 and8) (instrMemImm8 and8)),
        makeInstructionS 0x83 (Just 4) (decodeN16Imm8 (instrRegImm16 and16) (instrMemImm16 and16)),
        --XOR
        makeInstructionS 0x30 Nothing (decodeRm8 (instrRegToReg8RegToRm xor8) (instrRegToMem8 xor8)),
        makeInstructionS 0x31 Nothing (decodeRm16 (instrRegToReg16RegToRm xor16) (instrRegToMem16 xor16)),
        makeInstructionS 0x32 Nothing (decodeRm8 (instrRegToReg8RmToReg xor8) (instrMemToReg8 xor8)),
        makeInstructionS 0x33 Nothing (decodeRm16 (instrRegToReg16RmToReg xor16) (instrMemToReg16 xor16)),
        makeInstructionS 0x34 Nothing (decodeAcc8 al $ instrRegImm8 xor8),
        makeInstructionS 0x35 Nothing (decodeAcc16 ax $ instrRegImm16 xor16),
        makeInstructionS 0x80 (Just 6) (decodeN8Imm8 (instrRegImm8 xor8) (instrMemImm8 xor8)),
        makeInstructionS 0x81 (Just 6) (decodeN16Imm (instrRegImm16 xor16) (instrMemImm16 xor16)),
        makeInstructionS 0x82 (Just 6) (decodeN8Imm8 (instrRegImm8 xor8) (instrMemImm8 xor8)),
        makeInstructionS 0x83 (Just 6) (decodeN16Imm8 (instrRegImm16 xor16) (instrMemImm16 xor16)),
        --TEST
        makeInstructionS 0x84 Nothing (decodeRm8 (instrRegToReg8RegToRm test8) (instrRegToMem8 test8)),
        makeInstructionS 0x85 Nothing (decodeRm16 (instrRegToReg16RegToRm test16) (instrRegToMem16 test16)),
        makeInstructionS 0xA8 Nothing (decodeAcc8 al $ instrRegImm8 test8),
        makeInstructionS 0xA9 Nothing (decodeAcc16 ax $ instrRegImm16 test16),
        makeInstructionS 0xF6 (Just 0) (decodeN8Imm8 (instrRegImm8 test8) (instrMemImm8 test8)),
        makeInstructionS 0xF7 (Just 0) (decodeN16Imm (instrRegImm16 test16) (instrMemImm16 test16)),
        --SHL/SAL
        makeInstructionS 0xD0 (Just 4) (decodeN8 (instrReg8 $ rotateFuncOne8 shl8) (instrMem8 $ rotateFuncOne8 shl8)),
        makeInstructionS 0xD1 (Just 4) (decodeN16 (instrReg16 $ rotateFuncOne16 shl16) (instrMem16 $ rotateFuncOne16 shl16)),
        makeInstructionS 0xD2 (Just 4) (decodeN8 (instrRegVal8 $ rotateFuncCl8 shl8) (instrMemVal8 $ rotateFuncCl8 shl8)),
        makeInstructionS 0xD3 (Just 4) (decodeN16 (instrRegVal16 $ rotateFuncCl16 shl16) (instrMemVal16 $ rotateFuncCl16 shl16)),
        --SHR
        makeInstructionS 0xD0 (Just 5) (decodeN8 (instrReg8 $ rotateFuncOne8 shr8) (instrMem8 $ rotateFuncOne8 shr8)),
        makeInstructionS 0xD1 (Just 5) (decodeN16 (instrReg16 $ rotateFuncOne16 shr16) (instrMem16 $ rotateFuncOne16 shr16)),
        makeInstructionS 0xD2 (Just 5) (decodeN8 (instrRegVal8 $ rotateFuncCl8 shr8) (instrMemVal8 $ rotateFuncCl8 shr8)),
        makeInstructionS 0xD3 (Just 5) (decodeN16 (instrRegVal16 $ rotateFuncCl16 shr16) (instrMemVal16 $ rotateFuncCl16 shr16)),
        --SAR
        makeInstructionS 0xD0 (Just 7) (decodeN8 (instrReg8 $ rotateFuncOne8 sar8) (instrMem8 $ rotateFuncOne8 sar8)),
        makeInstructionS 0xD1 (Just 7) (decodeN16 (instrReg16 $ rotateFuncOne16 sar16) (instrMem16 $ rotateFuncOne16 sar16)),
        makeInstructionS 0xD2 (Just 7) (decodeN8 (instrRegVal8 $ rotateFuncCl8 sar8) (instrMemVal8 $ rotateFuncCl8 sar8)),
        makeInstructionS 0xD3 (Just 7) (decodeN16 (instrRegVal16 $ rotateFuncCl16 sar16) (instrMemVal16 $ rotateFuncCl16 sar16)),
        --ROL
        makeInstructionS 0xD0 (Just 0) (decodeN8 (instrReg8 $ rotateFuncOne8 rol8) (instrMem8 $ rotateFuncOne8 rol8)),
        makeInstructionS 0xD1 (Just 0) (decodeN16 (instrReg16 $ rotateFuncOne16 rol16) (instrMem16 $ rotateFuncOne16 rol16)),
        makeInstructionS 0xD2 (Just 0) (decodeN8 (instrRegVal8 $ rotateFuncCl8 rol8) (instrMemVal8 $ rotateFuncCl8 rol8)),
        makeInstructionS 0xD3 (Just 0) (decodeN16 (instrRegVal16 $ rotateFuncCl16 rol16) (instrMemVal16 $ rotateFuncCl16 rol16)),
        --ROR
        makeInstructionS 0xD0 (Just 1) (decodeN8 (instrReg8 $ rotateFuncOne8 ror8) (instrMem8 $ rotateFuncOne8 ror8)),
        makeInstructionS 0xD1 (Just 1) (decodeN16 (instrReg16 $ rotateFuncOne16 ror16) (instrMem16 $ rotateFuncOne16 ror16)),
        makeInstructionS 0xD2 (Just 1) (decodeN8 (instrRegVal8 $ rotateFuncCl8 ror8) (instrMemVal8 $ rotateFuncCl8 ror8)),
        makeInstructionS 0xD3 (Just 1) (decodeN16 (instrRegVal16 $ rotateFuncCl16 ror16) (instrMemVal16 $ rotateFuncCl16 ror16)),
        --RCL
        makeInstructionS 0xD0 (Just 2) (decodeN8 (instrReg8 $ rotateFuncOne8 rcl8) (instrMem8 $ rotateFuncOne8 rcl8)),
        makeInstructionS 0xD1 (Just 2) (decodeN16 (instrReg16 $ rotateFuncOne16 rcl16) (instrMem16 $ rotateFuncOne16 rcl16)),
        makeInstructionS 0xD2 (Just 2) (decodeN8 (instrRegVal8 $ rotateFuncCl8 rcl8) (instrMemVal8 $ rotateFuncCl8 rcl8)),
        makeInstructionS 0xD3 (Just 2) (decodeN16 (instrRegVal16 $ rotateFuncCl16 rcl16) (instrMemVal16 $ rotateFuncCl16 rcl16)),
        --RCR
        makeInstructionS 0xD0 (Just 3) (decodeN8 (instrReg8 $ rotateFuncOne8 rcr8) (instrMem8 $ rotateFuncOne8 rcr8)),
        makeInstructionS 0xD1 (Just 3) (decodeN16 (instrReg16 $ rotateFuncOne16 rcr16) (instrMem16 $ rotateFuncOne16 rcr16)),
        makeInstructionS 0xD2 (Just 3) (decodeN8 (instrRegVal8 $ rotateFuncCl8 rcr8) (instrMemVal8 $ rotateFuncCl8 rcr8)),
        makeInstructionS 0xD3 (Just 3) (decodeN16 (instrRegVal16 $ rotateFuncCl16 rcr16) (instrMemVal16 $ rotateFuncCl16 rcr16)),
        --NOT
        makeInstructionS 0xF6 (Just 2) (decodeN8 (instrReg8 not8) (instrMem8 not8)),
        makeInstructionS 0xF7 (Just 2) (decodeN16 (instrReg16 not16) (instrMem16 not16))
    ]
