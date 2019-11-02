module Instruction.Logical where

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Data.Bits (shiftR, complement, (.&.), (.|.), xor)

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
        --NOT
        makeInstructionS 0xF6 (Just 2) (decodeN8 (instrReg8 not8) (instrMem8 not8)),
        makeInstructionS 0xF7 (Just 2) (decodeN16 (instrReg16 not16) (instrMem16 not16))
    ]
