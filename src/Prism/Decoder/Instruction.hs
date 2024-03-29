{-# LANGUAGE FlexibleContexts #-}

module Prism.Decoder.Instruction where

import Data.Bits (shiftR, (.&.))

import Data.Bits (shiftL)
import Numeric (showHex)

import Prism.Cpu
import Prism.Instruction

import qualified Prism.Log as Log

import Prism.Decoder.Common

-------------------------------------------------------------------------------

decodeSkip :: PrismInstrFunc
decodeSkip _ = cpuUpdateIP 1

decodeWrong :: PrismInstrFunc
decodeWrong (b1, b2, b3, b4, b5, b6) = do
    csVal <- readOp cs
    ipVal <- readOp ip
    let offset = regsToOffset csVal ipVal
    Log.cpuLogT Error Log.CpuInt $ "Wrong instruction(cs=0x"
                                   ++ (showHex csVal "")
                                   ++ ",ip=0x" ++ (showHex ipVal "")
                                   ++ ",off=0x" ++ (showHex offset "")
                                   ++ "): op=0x" ++ (showHex b1 "")
                                   ++ ", 0x" ++ (showHex b2 "")
                                   ++ ", 0x" ++ (showHex b3 "")
                                   ++ ", 0x" ++ (showHex b4 "")
                                   ++ ", 0x" ++ (showHex b5 "")
                                   ++ ", 0x" ++ (showHex b6 "")
    cpuHalt
    where
        regsToOffset :: Uint16 -> Uint16 -> Int
        regsToOffset csVal ipVal = ((shiftL (fromIntegral csVal) 4) + (fromIntegral ipVal)) :: Int

decodeImplicit :: FuncImplicit -> PrismInstrFunc
decodeImplicit func _ =
    cpuUpdateIP 1 >> func

decodeImm1 :: (ImmDecoder v, OperandVal v) =>
    FuncImm1 v -> PrismInstrFunc
decodeImm1 func (_, b2, b3, _, _, _) =
    let imm = decodeImm b2 b3
        in
    cpuUpdateIP (1 + immLength imm) >> func imm

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
    cpuUpdateIP 5 >> func imm1 imm2

-------------------------------------------------------------------------------

decodeStR :: (OperandVal v, OperandReg a PrismM v) =>
    a -> FuncO1M a -> PrismInstrFunc
decodeStR reg func _ =
    cpuUpdateIP 1 >> func reg

{-# SPECIALISE decodeStR :: Reg8 -> FuncO1M Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStR :: Reg16 -> FuncO1M Reg16 -> PrismInstrFunc #-}

decodeStR2 :: (OperandVal v, OperandReg a PrismM v) =>
    a -> FuncO1M a -> PrismInstrFunc
decodeStR2 reg func _ =
    cpuUpdateIP 2 >> func reg

{-# SPECIALISE decodeStR2 :: Reg16 -> FuncO1M Reg16 -> PrismInstrFunc #-}

--Decode IMM and execute function with predefined REG
decodeStRI :: (ImmDecoder v, OperandVal v, OperandReg a PrismM v) =>
    a -> FuncOI1M a v -> PrismInstrFunc
decodeStRI reg func (_, b2, b3, _, _, _) =
    let imm = decodeImm b2 b3
        in
    cpuUpdateIP (1 + immLength imm) >> func reg imm

{-# SPECIALISE decodeStRI :: Reg8 -> FuncOI1M Reg8 Uint8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRI :: Reg16 -> FuncOI1M Reg16 Uint16 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRI :: RegSeg -> FuncOI1M RegSeg Uint16 -> PrismInstrFunc #-}

decodeStRR :: (OperandVal v, OperandReg a PrismM v) =>
    a -> a -> FuncO2M a a -> PrismInstrFunc
decodeStRR reg1 reg2 func _ =
     cpuUpdateIP 1 >> func reg1 reg2

{-# SPECIALISE decodeStRR :: Reg8 -> Reg8 -> FuncO2M Reg8 Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRR :: Reg16 -> Reg16 -> FuncO2M Reg16 Reg16 -> PrismInstrFunc #-}

decodeStRM :: (OperandVal v, OperandMem a1 PrismM v, OperandReg a2 PrismM v) =>
    a2 -> FuncO2M a1 a2 -> PrismInstrFunc
decodeStRM reg func (_, b2, b3, _, _, _) =
    let mem = decodeMemDirect $ getDisp16 b2 b3
        in
     cpuUpdateIP 3 >> func mem reg

{-# SPECIALISE decodeStRM :: Reg8 -> FuncO2M MemSeg8 Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRM :: Reg16 -> FuncO2M MemSeg16 Reg16 -> PrismInstrFunc #-}

decodeStRM8 :: Reg8 -> FuncO2M MemSeg8 Reg8 -> PrismInstrFunc
decodeStRM8 = decodeStRM

decodeStRM16 :: Reg16 -> FuncO2M MemSeg16 Reg16 -> PrismInstrFunc
decodeStRM16 = decodeStRM

-------------------------------------------------------------------------------

decodeNI :: (ImmDecoder v, OperandVal v, OperandReg a1 PrismM v, OperandMem a2 PrismM v) => 
    FuncOI1M a1 v -> FuncOI1M a2 v -> PrismInstrFunc
decodeNI freg fmem (b1, b2, b3, b4, b5, b6) =
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
                    cpuUpdateIP (4 + (immLength imm)) >> fmem mem imm
                _ ->
                    let mem = decodeMemSeg rm 0
                        imm = decodeImm b3 b4
                        in
                    cpuUpdateIP (2 + (immLength imm)) >> fmem mem imm
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMemSeg rm disp8
                imm = decodeImm b4 b5
                in
            cpuUpdateIP (3 + (immLength imm)) >> fmem mem imm
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMemSeg rm disp16
                imm = decodeImm b5 b6
                in
            cpuUpdateIP (4 + (immLength imm)) >> fmem mem imm
        0x03 ->
            let reg = decodeReg rm
                imm = decodeImm b3 b4
                in
            cpuUpdateIP (2 + (immLength imm)) >> freg reg imm

{-# SPECIALISE decodeNI :: FuncOI1M Reg8 Uint8 -> FuncOI1M MemSeg8 Uint8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeNI :: FuncOI1M Reg16 Uint16 -> FuncOI1M MemSeg16 Uint16 -> PrismInstrFunc #-}

decodeNI8 :: FuncOI1M Reg8 Uint8 -> FuncOI1M MemSeg8 Uint8 -> PrismInstrFunc
decodeNI8 = decodeNI

decodeNI16 :: FuncOI1M Reg16 Uint16 -> FuncOI1M MemSeg16 Uint16 -> PrismInstrFunc
decodeNI16 = decodeNI

-------------------------------------------------------------------------------

decodeNC :: (ImmDecoder v, OperandVal v, OperandReg a1 PrismM v, OperandMem a2 PrismM v) => 
    FuncOI1M a1 v -> FuncOI1M a2 v -> PrismInstrFunc
decodeNC freg fmem bytes =
    decodeNI8 freg8 fmem8 bytes
    where
        freg8 reg imm8 = freg (convertReg reg) $ signExtendWordN imm8
        fmem8 mem imm8 = fmem (convertMem mem) $ signExtendWordN imm8

{-# SPECIALISE decodeNC :: FuncOI1M Reg16 Uint16 -> FuncOI1M MemSeg16 Uint16 -> PrismInstrFunc #-}

decodeNC16 :: FuncOI1M Reg16 Uint16 -> FuncOI1M MemSeg16 Uint16 -> PrismInstrFunc
decodeNC16 = decodeNC

-------------------------------------------------------------------------------

decodeRM :: (OperandVal v, OperandReg a1 PrismM v, OperandReg a2 PrismM v, OperandMem a3 PrismM v) => 
    FuncO2M a2 a1 -> FuncO2M a3 a1 -> PrismInstrFunc
decodeRM freg fmem (b1, b2, b3, b4, _, _) =
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
                    cpuUpdateIP 4 >> fmem mem reg
                _ ->
                    let mem = decodeMemSeg rm 0
                        in
                    cpuUpdateIP 2 >> fmem mem reg
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMemSeg rm disp8
                in
            cpuUpdateIP 3 >> fmem mem reg
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMemSeg rm disp16
                in
            cpuUpdateIP 4 >> fmem mem reg
        0x03 ->
            let reg2 = decodeReg rm
                in
            cpuUpdateIP 2 >> freg reg2 reg

{-# SPECIALISE decodeRM :: FuncO2M Reg8 Reg8 -> FuncO2M MemSeg8 Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeRM :: FuncO2M Reg16 Reg16 -> FuncO2M MemSeg16 Reg16 -> PrismInstrFunc #-}
{-# SPECIALISE decodeRM :: FuncO2M Reg16 RegSeg -> FuncO2M MemSeg16 RegSeg -> PrismInstrFunc #-}

decodeRM8 :: FuncO2M Reg8 Reg8 -> FuncO2M MemSeg8 Reg8 -> PrismInstrFunc
decodeRM8 = decodeRM

decodeRM16 :: FuncO2M Reg16 Reg16 -> FuncO2M MemSeg16 Reg16 -> PrismInstrFunc
decodeRM16 = decodeRM

decodeRMS16 :: FuncO2M Reg16 RegSeg -> FuncO2M MemSeg16 RegSeg -> PrismInstrFunc
decodeRMS16 = decodeRM

-------------------------------------------------------------------------------

decodeN :: (OperandVal v, OperandReg a1 PrismM v, OperandMem a2 PrismM v) =>
    FuncO1M a1 -> FuncO1M a2 -> PrismInstrFunc
decodeN freg fmem (b1, b2, b3, b4, _, _) =
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
                    cpuUpdateIP 4 >> fmem mem
                _ ->
                    let mem = decodeMemSeg rm 0
                        in
                    cpuUpdateIP 2 >> fmem mem
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMemSeg rm disp8
                in
            cpuUpdateIP 3 >> fmem mem
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMemSeg rm disp16
                in
            cpuUpdateIP 4 >> fmem mem
        0x03 ->
            let reg = decodeReg rm
                in
            cpuUpdateIP 2 >> freg reg

{-# SPECIALISE decodeN :: FuncO1M Reg8 -> FuncO1M MemSeg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeN :: FuncO1M Reg16 -> FuncO1M MemSeg16 -> PrismInstrFunc #-}

decodeN8 :: FuncO1M Reg8 -> FuncO1M MemSeg8 -> PrismInstrFunc
decodeN8 = decodeN

decodeN16 :: FuncO1M Reg16 -> FuncO1M MemSeg16 -> PrismInstrFunc
decodeN16 = decodeN

-------------------------------------------------------------------------------
