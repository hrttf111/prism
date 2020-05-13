module Prism.Decoder.Instruction where

import Data.Bits (shiftR)

import Prism.Cpu
import Prism.Instruction

import Prism.Decoder.Common

-------------------------------------------------------------------------------

{-
decodeDemux :: Array Uint8 PrismInstrFunc -> PrismInstrFunc
decodeDemux commands instr@(_, b2, _, _, _, _) ctx =
    func instr ctx
    where
        rm = shiftR (b2 .&. 0x38) 3
        func = commands ! rm
-}
-------------------------------------------------------------------------------

decodeEmpty :: PrismInstrFunc
decodeEmpty _ = updateIP 1

-------------------------------------------------------------------------------

decodeImplicit :: FuncImplicit -> PrismInstrFunc
decodeImplicit func _ =
    updateIP 1 >> func

decodeImm1 :: (ImmDecoder v, OperandVal v) =>
    FuncImm1 v -> PrismInstrFunc
decodeImm1 func (_, b2, b3, _, _, _) =
    let imm = decodeImm b2 b3
        in
    updateIP (1 + immLength imm) >> func imm

{-# SPECIALISE decodeImm1 :: FuncImm1 Imm8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeImm1 :: FuncImm1 Imm16 -> PrismInstrFunc #-}

decodeImm8 :: FuncImm1 Imm8 -> PrismInstrFunc
decodeImm8 = decodeImm1

decodeImm16 :: FuncImm1 Imm16 -> PrismInstrFunc
decodeImm16 = decodeImm1

decodeImm32 :: FuncImm2 Imm16 -> PrismInstrFunc
decodeImm32 func (_, b2, b3, b4, b5, _) =
    let imm1 = getImm16 b2 b3
        imm2 = getImm16 b4 b5
        in
    updateIP 5 >> func imm1 imm2

{-
-------------------------------------------------------------------------------

decodeStR :: (OperandVal b, OperandReg a b) =>
    a -> FuncO1M a -> PrismInstrFunc 
decodeStR reg func _ ctx =
    func ctx reg >>= updateIP 1

{-# SPECIALISE decodeStR :: Reg8 -> FuncO1M Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStR :: Reg16 -> FuncO1M Reg16 -> PrismInstrFunc #-}

--Decode IMM and execute function with predefined REG
decodeStRI :: (ImmDecoder b, OperandVal b, OperandReg a b) =>
    a -> FuncOI1M a b -> PrismInstrFunc
decodeStRI reg func (_, b2, b3, _, _, _) ctx =
    let imm = decodeImm b2 b3 
        in
    func ctx reg imm >>= updateIP (1 + immLength imm)

{-# SPECIALISE decodeStRI :: Reg8 -> FuncOI1M Reg8 Uint8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRI :: Reg16 -> FuncOI1M Reg16 Uint16 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRI :: RegSeg -> FuncOI1M RegSeg Uint16 -> PrismInstrFunc #-}

decodeStRR :: (OperandVal b, OperandReg a b) =>
    a -> a -> FuncO2M a a -> PrismInstrFunc
decodeStRR reg1 reg2 func _ ctx =
    func ctx reg1 reg2 >>= updateIP 1

{-# SPECIALISE decodeStRR :: Reg8 -> Reg8 -> FuncO2M Reg8 Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRR :: Reg16 -> Reg16 -> FuncO2M Reg16 Reg16 -> PrismInstrFunc #-}

decodeStRM :: (OperandVal b, OperandMem a1 b, OperandReg a2 b) =>
    a2 -> FuncO2M a1 a2 -> PrismInstrFunc
decodeStRM reg func (_, b2, b3, _, _, _) ctx =
    let mem = decodeMemDirect $ getDisp16 b2 b3
        in
    func ctx mem reg >>= updateIP 3

{-# SPECIALISE decodeStRM :: Reg8 -> FuncO2M Mem8 Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRM :: Reg16 -> FuncO2M Mem16 Reg16 -> PrismInstrFunc #-}

decodeStRM8 :: Reg8 -> FuncO2M Mem8 Reg8 -> PrismInstrFunc
decodeStRM8 = decodeStRM

decodeStRM16 :: Reg16 -> FuncO2M Mem16 Reg16 -> PrismInstrFunc
decodeStRM16 = decodeStRM

-------------------------------------------------------------------------------

decodeNI :: (ImmDecoder b, OperandVal b, OperandReg a1 b, OperandMem a2 b) => 
    FuncOI1M a1 b -> FuncOI1M a2 b -> PrismInstrFunc
decodeNI freg fmem (b1, b2, b3, b4, b5, b6) ctx = 
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        in
    case mod of
        0x00 ->
            case rm of
                0x06 ->
                    let disp16 = getDisp16 b3 b4
                        imm = decodeImm b5 b6
                        mem = decodeMemDirect disp16
                        in
                    fmem ctx mem imm >>= updateIP (4 + (immLength imm))
                _ ->
                    let mem = decodeMem1 rm 0
                        imm = decodeImm b3 b4
                        in
                    fmem ctx mem imm >>= updateIP (2 + (immLength imm))
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem1 rm disp8
                imm = decodeImm b4 b5
                in
            fmem ctx mem imm >>= updateIP (3 + (immLength imm))
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem1 rm disp16
                imm = decodeImm b5 b6
                in
            fmem ctx mem imm >>= updateIP (4 + (immLength imm))
        0x03 ->
            let reg = decodeReg rm
                imm = decodeImm b3 b4
                in
            freg ctx reg imm >>= updateIP (2 + (immLength imm))

{-# SPECIALISE decodeNI :: FuncOI1M Reg8 Uint8 -> FuncOI1M Mem8 Uint8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeNI :: FuncOI1M Reg16 Uint16 -> FuncOI1M Mem16 Uint16 -> PrismInstrFunc #-}

decodeNI8 :: FuncOI1M Reg8 Uint8 -> FuncOI1M Mem8 Uint8 -> PrismInstrFunc
decodeNI8 = decodeNI

decodeNI16 :: FuncOI1M Reg16 Uint16 -> FuncOI1M Mem16 Uint16 -> PrismInstrFunc
decodeNI16 = decodeNI

-------------------------------------------------------------------------------

decodeNC :: (ImmDecoder b, OperandVal b, OperandReg a1 b, OperandMem a2 b) => 
    FuncOI1M a1 b -> FuncOI1M a2 b -> PrismInstrFunc
decodeNC freg fmem bytes ctx = 
    decodeNI8 freg8 fmem8 bytes ctx
    where
        freg8 ctx reg imm8 = freg ctx (convertReg reg) $ signExterndWordN imm8
        fmem8 ctx mem imm8 = fmem ctx (convertMem mem) $ signExterndWordN imm8

{-# SPECIALISE decodeNC :: FuncOI1M Reg16 Uint16 -> FuncOI1M Mem16 Uint16 -> PrismInstrFunc #-}

decodeNC16 :: FuncOI1M Reg16 Uint16 -> FuncOI1M Mem16 Uint16 -> PrismInstrFunc
decodeNC16 = decodeNC

-------------------------------------------------------------------------------

decodeRM :: (OperandVal b, OperandReg a1 b, OperandReg a2 b, OperandMem a3 b) => 
    FuncO2M a2 a1 -> FuncO2M a3 a1 -> PrismInstrFunc
decodeRM freg fmem (b1, b2, b3, b4, _, _) ctx =
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        reg = decodeReg $ shiftR (modrm .&. 0x38) 3
        in
    case mod of
        0x00 ->
            case rm of
                0x06 ->
                    let disp16 = getDisp16 b3 b4
                        mem = decodeMemDirect disp16
                        in
                    fmem ctx mem reg >>= updateIP 4
                _ ->
                    let mem = decodeMem1 rm 0
                        in
                    fmem ctx mem reg >>= updateIP 2
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem1 rm disp8
                in
            fmem ctx mem reg >>= updateIP 3
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem1 rm disp16
                in
            fmem ctx mem reg >>= updateIP 4
        0x03 ->
            let reg2 = decodeReg rm
                in
            freg ctx reg2 reg >>= updateIP 2

{-# SPECIALISE decodeRM :: FuncO2M Reg8 Reg8 -> FuncO2M Mem8 Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeRM :: FuncO2M Reg16 Reg16 -> FuncO2M Mem16 Reg16 -> PrismInstrFunc #-}
{-# SPECIALISE decodeRM :: FuncO2M Reg16 RegSeg -> FuncO2M Mem16 RegSeg -> PrismInstrFunc #-}

decodeRM8 :: FuncO2M Reg8 Reg8 -> FuncO2M Mem8 Reg8 -> PrismInstrFunc
decodeRM8 = decodeRM

decodeRM16 :: FuncO2M Reg16 Reg16 -> FuncO2M Mem16 Reg16 -> PrismInstrFunc
decodeRM16 = decodeRM

decodeRMS16 :: FuncO2M Reg16 RegSeg -> FuncO2M Mem16 RegSeg -> PrismInstrFunc
decodeRMS16 = decodeRM

-------------------------------------------------------------------------------

decodeN :: (OperandVal b, OperandReg a1 b, OperandMem a2 b) =>
    FuncO1M a1 -> FuncO1M a2 -> PrismInstrFunc
decodeN freg fmem (b1, b2, b3, b4, _, _) ctx =
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        in
    case mod of
        0x00 ->
            case rm of
                0x06 ->
                    let disp16 = getDisp16 b3 b4
                        mem = decodeMemDirect disp16
                        in
                    fmem ctx mem >>= updateIP 4
                _ ->
                    let mem = decodeMem1 rm 0
                        in
                    fmem ctx mem >>= updateIP 2
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem1 rm disp8
                in
            fmem ctx mem >>= updateIP 3
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem1 rm disp16
                in
            fmem ctx mem >>= updateIP 4
        0x03 ->
            let reg = decodeReg rm
                in
            freg ctx reg >>= updateIP 2

{-# SPECIALISE decodeN :: FuncO1M Reg8 -> FuncO1M Mem8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeN :: FuncO1M Reg16 -> FuncO1M Mem16 -> PrismInstrFunc #-}

decodeN8 :: FuncO1M Reg8 -> FuncO1M Mem8 -> PrismInstrFunc
decodeN8 = decodeN

decodeN16 :: FuncO1M Reg16 -> FuncO1M Mem16 -> PrismInstrFunc
decodeN16 = decodeN

-------------------------------------------------------------------------------
-}
