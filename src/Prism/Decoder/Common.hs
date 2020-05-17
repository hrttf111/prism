{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Prism.Decoder.Common where

import Data.Bits (shiftL)

import Prism.Cpu

-------------------------------------------------------------------------------

type InstrBytes = (Uint8, Uint8, Uint8, Uint8, Uint8, Uint8)

type PrismInstrFunc = InstrBytes -> PrismM ()

-------------------------------------------------------------------------------

class ImmDecoder a where
    decodeImm :: Uint8 -> Uint8 -> a
    immLength :: a -> Uint16

-------------------------------------------------------------------------------

instance ImmDecoder Imm8 where
    decodeImm b1 _ = getImm8 b1
    immLength _ = 1

instance ImmDecoder Imm16 where
    decodeImm = getImm16
    immLength _ = 2

getImm8 :: Uint8 -> Imm8
getImm8 = id

getImm16 :: Uint8 -> Uint8 -> Imm16
getImm16 lo hi = (+) (fromIntegral lo :: Imm16) $ shiftL (fromIntegral hi :: Imm16) 8

-------------------------------------------------------------------------------

getDisp8 :: Uint8 -> Disp
getDisp8 lo = fromIntegral lo :: Disp

getDisp16 :: Uint8 -> Uint8 -> Disp
getDisp16 lo hi = (+) (fromIntegral lo :: Disp) $ shiftL (fromIntegral hi :: Disp) 8

-------------------------------------------------------------------------------

instance RegDecoder Reg8 where
    decodeReg = Reg8
    decodeRegVal (Reg8 v) = v

instance RegDecoder Reg16 where
    decodeReg = Reg16
    decodeRegVal (Reg16 v) = v

instance RegDecoder RegSeg where
    decodeReg = RegSeg
    decodeRegVal (RegSeg v) = v

convertReg :: (RegDecoder a1, RegDecoder a2) =>
    a1 -> a2
convertReg = decodeReg . decodeRegVal

-------------------------------------------------------------------------------

instance MemDecoder MemSeg8 where
    decodeMemSeg v off = MemSeg8 $ decodeMem v off
    decodeMemDirect = MemSeg8 . MemDirect

instance MemDecoder MemSeg16 where
    decodeMemSeg v off = MemSeg16 $ decodeMem v off
    decodeMemDirect = MemSeg16 . MemDirect

-- R/M -> Disp -> Mem
decodeMem :: Uint8 -> Disp -> MemSeg
decodeMem 0 = MemBxSi 
decodeMem 1 = MemBxDi
decodeMem 2 = MemBpSi
decodeMem 3 = MemBpDi
decodeMem 4 = MemSi
decodeMem 5 = MemDi
decodeMem 6 = MemBp
decodeMem 7 = MemBx
decodeMem _ = MemDirect

convertMem :: (MemDecoder a1, MemDecoder a2) =>
    a1 -> a2
convertMem = wrapMemSeg . unwrapMemSeg

-------------------------------------------------------------------------------
