module Main where

import Data.Bits((.&.), (.|.), shift)

import Prism
import PrismDecoder

import Control.Monad.Trans (lift, liftIO)

import Foreign.Marshal.Alloc
import Foreign.Ptr

testDecoder = do
    putStrLn "1"
    runPrism $ decodeN8Imm8 freg fmem instr ctx1
    where
        ctx1 = Ctx nullPtr nullPtr
        rm = (0x80 .|. 0x0 .|. 0x2) :: Uint8
        --rm = (0xE0 .|. 0x0 .|. 0x2) :: Uint8
        instr = (0x80, rm, 0x7F, 0x10, 0x99, 0x00)
        freg ctx reg imm = liftIO $ do
            putStrLn $ show reg
            putStrLn $ show imm
            return $ ctx1
        fmem ctx mem imm = liftIO $ do
            putStrLn $ show mem 
            putStrLn $ show imm
            return $ ctx1

main :: IO ()
main = do
    putStrLn "Start"
    testDecoder
    return ()
