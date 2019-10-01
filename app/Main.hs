module Main where

import Data.Bits((.&.), (.|.), shift)

import Prism
import PrismDecoder

testDecoder = decodeN8Imm8 ctx freg fmem instr
    where
        ctx = Ctx ""
        rm = (0x80 .|. 0x0 .|. 0x2) :: Uint8
        --rm = (0xE0 .|. 0x0 .|. 0x2) :: Uint8
        instr = (0x80, rm, 0x7F, 0x10, 0x99, 0x00)
        freg ctx reg imm = Ctx $ (++) (show reg) (show imm)
        fmem ctx mem imm = Ctx $ (++) (show mem) ""--(show imm)

main :: IO ()
main = do
    putStrLn $ show testDecoder
    return ()
