module Main where

import Data.Bits((.&.), (.|.), shift)

import Prism
import PrismDecoder

import Control.Monad.Trans (lift, liftIO, MonadIO)

import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.Marshal.Alloc
import Foreign.Ptr

readMem8 :: MonadIO m => Ctx -> Int -> m Uint8
readMem8 ctx offset = liftIO $ peekByteOff (ctxMem ctx) offset

writeMem8 :: MonadIO m => Ctx -> Uint8 -> Int -> m ()
writeMem8 ctx val offset = liftIO $ pokeByteOff (ctxMem ctx) offset val

testDecoder = do
    putStrLn "1"
    ptrReg <- callocBytes 64
    ptrMem <- callocBytes 65000
    let ctx1 = Ctx ptrReg ptrMem 
    runPrism $ decodeN8Imm8 freg fmem instr ctx1
    where
        rm = (0x80 .|. 0x0 .|. 0x2) :: Uint8
        --rm = (0xE0 .|. 0x0 .|. 0x2) :: Uint8
        instr = (0x80, rm, 0x7F, 0x10, 0x99, 0x00)
        freg ctx reg imm = liftIO $ do
            putStrLn $ show reg
            putStrLn $ show imm
            return $ ctx
        fmem ctx mem imm = liftIO $ do
            putStrLn $ show mem 
            writeMem8 ctx imm 1000
            memVal <- readMem8 ctx 1000
            putStrLn $ "Mem val: " ++ (show memVal)
            putStrLn $ show imm
            return $ ctx

main :: IO ()
main = do
    putStrLn "Start"
    testDecoder
    return ()
