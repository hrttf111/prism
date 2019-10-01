{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PrismDecoder where

import Data.Bits((.&.), (.|.), shiftR, shiftL)

import Prism

type Imm8 = Uint8

data Reg = AL | AH | AX |
           BL | BH | BX |
           CL | CH | CX |
           DL | DH | DX |
           SP |
           BP |
           SI |
           DI |
           RegUnknown deriving (Show)

data Mem = MemBxSi Int |
           MemBxDi Int |
           MemBpSi Int |
           MemBpDi Int |
           MemSi Int |
           MemDi Int |
           MemBp Int |
           MemBx Int |
           MemUnknown Int deriving (Show)

type FuncRegImm8 = Ctx -> Reg -> Imm8 -> Ctx
type FuncMemImm8 = Ctx -> Mem -> Imm8 -> Ctx

-- R/M -> Reg
decodeReg8 :: Uint8 -> Reg
decodeReg8 0 = AL
decodeReg8 1 = CL
decodeReg8 2 = DL
decodeReg8 3 = BL
decodeReg8 4 = AH
decodeReg8 5 = CH
decodeReg8 6 = DH
decodeReg8 7 = BH
decodeReg8 _ = RegUnknown

-- R/M -> Disp -> Mem
decodeMem :: Uint8 -> Int -> Mem
decodeMem 0 = MemBxSi 
decodeMem 1 = MemBxDi
decodeMem 2 = MemBpSi
decodeMem 3 = MemBpDi
decodeMem 4 = MemSi
decodeMem 5 = MemDi
decodeMem 6 = MemBp
decodeMem 7 = MemBx
decodeMem _ = MemUnknown

getDisp8 :: Uint8 -> Int
getDisp8 lo = fromIntegral lo :: Int

getDisp16 :: Uint8 -> Uint8 -> Int
getDisp16 lo hi = (+) (fromIntegral lo :: Int) $ shiftL (fromIntegral hi :: Int) 8

decodeN8Imm8 :: Ctx -> FuncRegImm8 -> FuncMemImm8 -> InstrBytes -> Ctx
decodeN8Imm8 ctx freg fmem (b1, b2, b3, b4, b5, b6) = 
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        imm8 = b3 :: Imm8
        in
    case mod of
        0x00 ->
            let mem = decodeMem rm 0
                in
            fmem ctx mem imm8
        0x01 -> 
            let disp8 = getDisp8 b4
                mem = decodeMem rm disp8
                in
            fmem ctx mem imm8
        0x02 ->
            let disp16 = getDisp16 b4 b5 
                mem = decodeMem rm disp16
                in
            fmem ctx mem imm8
        0x03 ->
            let reg = decodeReg8 rm
                in
            freg ctx reg imm8
