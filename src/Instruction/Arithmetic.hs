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

addRegImm8 :: Ctx -> Reg8 -> Imm8 -> PrismM
addRegImm8 = instrRegImm8 add8

addRegImm16 :: Ctx -> Reg16 -> Imm16 -> PrismM
addRegImm16 = instrRegImm16 add16

addRegToReg8 :: Ctx -> Reg8 -> Reg8 -> PrismM
addRegToReg8 = instrRegToReg8 add8

addRegToReg16 :: Ctx -> Reg16 -> Reg16 -> PrismM
addRegToReg16 = instrRegToReg16 add16

addRegToMem8 :: Ctx -> Mem -> Reg8 -> PrismM
addRegToMem8 = instrRegToMem8 add8

addRegToMem16 :: Ctx -> Mem -> Reg16 -> PrismM
addRegToMem16 = instrRegToMem16 add16

addMemToReg8 :: Ctx -> Mem -> Reg8 -> PrismM
addMemToReg8 = instrMemToReg8 add8

addMemToReg16 :: Ctx -> Mem -> Reg16 -> PrismM
addMemToReg16 = instrMemToReg16 add16

addMemImm8 :: Ctx -> Mem -> Imm8 -> PrismM
addMemImm8 = instrMemImm8 add8

addMemImm16 :: Ctx -> Mem -> Imm16 -> PrismM
addMemImm16 = instrMemImm16 add16

-------------------------------------------------------------------------------

arithmeticInstrList = [
        --ADD
        makeInstructionS 0x00 Nothing (decodeRm8 addRegToReg8 addRegToMem8),
        makeInstructionS 0x01 Nothing (decodeRm16 addRegToReg16 addRegToMem16),
        makeInstructionS 0x02 Nothing (decodeRm8 addRegToReg8 addMemToReg8),
        makeInstructionS 0x03 Nothing (decodeRm16 addRegToReg16 addMemToReg16),
        makeInstructionS 0x04 Nothing (decodeAcc8 al addRegImm8),
        makeInstructionS 0x05 Nothing (decodeAcc16 ax addRegImm16),
        makeInstructionS 0x80 (Just 0) (decodeN8Imm8 addRegImm8 addMemImm8),
        makeInstructionS 0x81 (Just 0) (decodeN16Imm addRegImm16 addMemImm16),
        makeInstructionS 0x82 (Just 0) (decodeN8Imm8 addRegImm8 addMemImm8),
        makeInstructionS 0x83 (Just 0) (decodeN16Imm8 addRegImm16 addMemImm16)
    ]
