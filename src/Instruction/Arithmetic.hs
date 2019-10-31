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

--
addRegImm8 :: Ctx -> Reg8 -> Imm8 -> PrismM
addRegImm8 = instrRegImm8 add8

addRegToReg8 :: Ctx -> Reg8 -> Reg8 -> PrismM
addRegToReg8 = instrRegToReg8 add8

addRegToMem8 :: Ctx -> Mem -> Reg8 -> PrismM
addRegToMem8 = instrRegToMem8 add8

addMemToReg8 :: Ctx -> Mem -> Reg8 -> PrismM
addMemToReg8 = instrMemToReg8 add8

addMemImm8 :: Ctx -> Mem -> Imm8 -> PrismM
addMemImm8 = instrMemImm8 add8

-------------------------------------------------------------------------------

arithmeticInstrList = [
        --ADD
        makeInstructionS 0x00 Nothing (decodeRm8 addRegToReg8 addRegToMem8),
        --16
        makeInstructionS 0x02 Nothing (decodeRm8 addRegToReg8 addMemToReg8),
        --16
        makeInstructionS 0x04 Nothing (decodeAcc8 al addRegImm8),
        --16
        makeInstructionS 0x80 (Just 0) (decodeN8Imm8 addRegImm8 addMemImm8),
        --16
        makeInstructionS 0x82 (Just 0) (decodeN8Imm8 addRegImm8 addMemImm8)
    ]
