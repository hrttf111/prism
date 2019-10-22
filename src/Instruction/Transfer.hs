module Instruction.Transfer where

import Control.Monad.Trans (lift, liftIO, MonadIO)

import Prism
import PrismDecoder
import PrismCpu

-------------------------------------------------------------------------------

mov8 :: Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
mov8 ctx source _ = (ctx, source)

mov16 :: Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)
mov16 ctx source _ = (ctx, source)

movRegImm8 :: Ctx -> Reg8 -> Imm8 -> PrismM
movRegImm8 = instrRegImm8 mov8

movRegImm16 :: Ctx -> Reg16 -> Imm16 -> PrismM
movRegImm16 = instrRegImm16 mov16

movSegImm16 :: Ctx -> RegSeg -> Imm16 -> PrismM
movSegImm16 = instrSegImm16 mov16

movRegToSeg16 :: Ctx -> Reg16 -> RegSeg -> PrismM
movRegToSeg16 = instrRegToSeg16 mov16

movSegToReg16 :: Ctx -> Reg16 -> RegSeg -> PrismM
movSegToReg16 = instrSegToReg16 mov16

movMemToSeg16 :: Ctx -> Mem -> RegSeg -> PrismM 
movMemToSeg16 = instrMemToSeg16 mov16

movSegToMem16 :: Ctx -> Mem -> RegSeg -> PrismM
movSegToMem16 = instrSegToMem16 mov16

movRegToReg8 :: Ctx -> Reg8 -> Reg8 -> PrismM
movRegToReg8 = instrRegToReg8 mov8

movRegToReg16 :: Ctx -> Reg16 -> Reg16 -> PrismM
movRegToReg16 = instrRegToReg16 mov16

movRegToMem8 :: Ctx -> Mem -> Reg8 -> PrismM
movRegToMem8 = instrRegToMem8 mov8

movRegToMem16 :: Ctx -> Mem -> Reg16 -> PrismM
movRegToMem16 = instrRegToMem16 mov16

movMemToReg8 :: Ctx -> Mem -> Reg8 -> PrismM
movMemToReg8 = instrMemToReg8 mov8

movMemToReg16 :: Ctx -> Mem -> Reg16 -> PrismM
movMemToReg16 = instrMemToReg16 mov16

movMemImm8 :: Ctx -> Mem -> Imm8 -> PrismM
movMemImm8 = instrMemImm8 mov8

movMemImm16 :: Ctx -> Mem -> Imm16 -> PrismM
movMemImm16 = instrMemImm16 mov16

-------------------------------------------------------------------------------

xchgRegReg8 :: FuncRegReg8
xchgRegReg8 ctx reg1 reg2 = do
    valReg1 <- readReg8 memReg reg1
    valReg2 <- readReg8 memReg reg2
    writeReg8 memReg reg1 valReg2
    writeReg8 memReg reg2 valReg1
    return ctx
    where
        memReg = ctxReg ctx

xchgRegReg16 :: FuncRegReg16
xchgRegReg16 ctx reg1 reg2 = do
    valReg1 <- readReg16 memReg reg1
    valReg2 <- readReg16 memReg reg2
    writeReg16 memReg reg1 valReg2
    writeReg16 memReg reg2 valReg1
    return ctx
    where
        memReg = ctxReg ctx

xchgMemReg8 :: FuncMemReg8
xchgMemReg8 ctx mem reg = do
    valReg <- readReg8 memReg reg
    valMem <- readMem8 memReg memMain regSeg mem
    writeMem8 memReg memMain regSeg mem valReg
    writeReg8 memReg reg valMem
    return ctx
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = findRegSegData ctx

xchgMemReg16 :: FuncMemReg16
xchgMemReg16 ctx mem reg = do
    valReg <- readReg16 memReg reg
    valMem <- readMem16 memReg memMain regSeg mem
    writeMem16 memReg memMain regSeg mem valReg
    writeReg16 memReg reg valMem
    return ctx
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = findRegSegData ctx

-------------------------------------------------------------------------------

transferInstrList = [
        --MOV
        makeInstructionS 0x88 Nothing (decodeRm8 movRegToReg8 movRegToMem8),
        makeInstructionS 0x89 Nothing (decodeRm16 movRegToReg16 movRegToMem16),
        makeInstructionS 0x8A Nothing (decodeRm8 movRegToReg8 movMemToReg8),
        makeInstructionS 0x8B Nothing (decodeRm16 movRegToReg16 movMemToReg16),
        makeInstructionS 0x8C Nothing (decodeRmS16 movSegToReg16 movSegToMem16),
        makeInstructionS 0x8E Nothing (decodeRmS16 movRegToSeg16 movMemToSeg16),
        makeInstructionS 0xA0 Nothing (decodeAccMem8 al movMemToReg8),
        makeInstructionS 0xA1 Nothing (decodeAccMem16 ax movMemToReg16),
        makeInstructionS 0xA2 Nothing (decodeAccMem8 al movRegToMem8),
        makeInstructionS 0xA3 Nothing (decodeAccMem16 ax movRegToMem16),
        makeInstructionS 0xB0 Nothing (decodeAcc8 al movRegImm8),
        makeInstructionS 0xB1 Nothing (decodeAcc8 cl movRegImm8),
        makeInstructionS 0xB2 Nothing (decodeAcc8 dl movRegImm8),
        makeInstructionS 0xB3 Nothing (decodeAcc8 bl movRegImm8),
        makeInstructionS 0xB4 Nothing (decodeAcc8 ah movRegImm8),
        makeInstructionS 0xB5 Nothing (decodeAcc8 ch movRegImm8),
        makeInstructionS 0xB6 Nothing (decodeAcc8 dh movRegImm8),
        makeInstructionS 0xB7 Nothing (decodeAcc8 bh movRegImm8),
        makeInstructionS 0xB8 Nothing (decodeAcc16 ax movRegImm16),
        makeInstructionS 0xB9 Nothing (decodeAcc16 cx movRegImm16),
        makeInstructionS 0xBA Nothing (decodeAcc16 dx movRegImm16),
        makeInstructionS 0xBB Nothing (decodeAcc16 bx movRegImm16),
        makeInstructionS 0xBC Nothing (decodeAcc16 sp movRegImm16),
        makeInstructionS 0xBD Nothing (decodeAcc16 bp movRegImm16),
        makeInstructionS 0xBE Nothing (decodeAcc16 si movRegImm16),
        makeInstructionS 0xBF Nothing (decodeAcc16 di movRegImm16),
        makeInstructionS 0xC6 (Just 0) (decodeN8Imm8 movRegImm8 movMemImm8),
        makeInstructionS 0xC7 (Just 0) (decodeN16Imm movRegImm16 movMemImm16),
        --XCHG
        makeInstructionS 0x86 Nothing (decodeRm8 xchgRegReg8 xchgMemReg8),
        makeInstructionS 0x87 Nothing (decodeRm16 xchgRegReg16 xchgMemReg16),
        --makeInstructionS 0x90 Nothing (decodeAccReg16 ax ax xchgRegReg16), NOP
        makeInstructionS 0x91 Nothing (decodeAccReg16 ax cx xchgRegReg16),
        makeInstructionS 0x92 Nothing (decodeAccReg16 ax dx xchgRegReg16),
        makeInstructionS 0x93 Nothing (decodeAccReg16 ax bx xchgRegReg16),
        makeInstructionS 0x94 Nothing (decodeAccReg16 ax sp xchgRegReg16),
        makeInstructionS 0x95 Nothing (decodeAccReg16 ax bp xchgRegReg16),
        makeInstructionS 0x96 Nothing (decodeAccReg16 ax si xchgRegReg16),
        makeInstructionS 0x97 Nothing (decodeAccReg16 ax di xchgRegReg16)
    ]

