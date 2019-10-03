{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PrismDecoder where

import Data.Bits ((.&.), (.|.), shiftR, shiftL)

import Data.Array (Array, array, accumArray, (!))
import Control.Monad.Trans (lift, liftIO, MonadIO)

import Foreign.Ptr
import Foreign.Storable (peekByteOff, pokeByteOff)

import Prism

al = Reg8 0
cl = Reg8 1
dl = Reg8 2
bl = Reg8 3
ah = Reg8 4
ch = Reg8 5
dh = Reg8 6
bh = Reg8 7

ax = Reg16 0
cx = Reg16 1
dx = Reg16 2
bx = Reg16 3
sp = Reg16 4
bp = Reg16 5
si = Reg16 6
di = Reg16 7

es = RegSeg 0
cs = RegSeg 1
ss = RegSeg 2
ds = RegSeg 3

data Mem = MemBxSi Int |
           MemBxDi Int |
           MemBpSi Int |
           MemBpDi Int |
           MemSi Int |
           MemDi Int |
           MemBp Int |
           MemBx Int |
           MemUnknown Int deriving (Show)

type PrismInstrFunc = InstrBytes -> Ctx -> PrismCtx IO Ctx

data PrismInstruction = PrismInstruction {
        instrOpcode :: Uint8,
        instrFunc :: PrismInstrFunc,
        instrDemux :: Array Uint8 PrismInstrFunc
    }

data PrismDecoder = PrismDecoder {
        decInstr :: Array Uint8 PrismInstruction
    }

makeDecoder :: [PrismInstruction] -> PrismDecoder
makeDecoder list =
    let arr = accumArray acc instrEmpty (0, 255) listF
        acc _ i = i
        instrEmpty = makeInstructionS 0 decodeEmpty
        listF = map (\i -> (instrOpcode i, i)) list
        in
    PrismDecoder arr

makeInstructionS :: Uint8 -> PrismInstrFunc -> PrismInstruction
makeInstructionS opcode func = makeInstruction opcode [(0, func)]

makeInstruction :: Uint8 -> [(Uint8, PrismInstrFunc)] -> PrismInstruction
makeInstruction opcode [] = 
    PrismInstruction opcode decodeEmpty $ array (0, 0) []
makeInstruction opcode [(_, func)] = 
    PrismInstruction opcode func $ array (0, 0) []
makeInstruction opcode funcs = 
    let acc _ i = i
        arr = accumArray acc decodeEmpty (0, 7) funcs
        func = decodeDemux arr
        in
    PrismInstruction opcode func arr 

decodeEmpty :: PrismInstrFunc
decodeEmpty _ ctx = liftIO $ putStrLn "Empty" >> return ctx

decodeDemux :: Array Uint8 PrismInstrFunc -> PrismInstrFunc
decodeDemux _ _ ctx = return ctx

type FuncRegImm8 = Ctx -> Reg8 -> Imm8 -> PrismCtx IO Ctx
type FuncMemImm8 = Ctx -> Mem -> Imm8 -> PrismCtx IO Ctx

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

decodeN8Imm8 :: FuncRegImm8 -> FuncMemImm8 -> InstrBytes -> Ctx -> PrismCtx IO Ctx
decodeN8Imm8 freg fmem (b1, b2, b3, b4, b5, b6) ctx = 
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
            let reg = Reg8 rm
                in
            freg ctx reg imm8

decodeList :: PrismDecoder -> Ctx -> [InstrBytes] -> PrismCtx IO Ctx
decodeList _ ctx [] = return ctx
decodeList dec ctx (x:xs) = do
    ctx1 <- instr x ctx
    decodeList dec ctx1 xs
    where
        (b1, _, _, _, _, _) = x
        instr = instrFunc $ (decInstr dec) ! b1
