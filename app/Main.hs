module Main where

import Data.Bits((.&.), (.|.), shift)

import Prism
import PrismDecoder
import PrismCpu
import PrismIO
import PrismShow

import Control.Monad.Trans (lift, liftIO, MonadIO)

import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.Marshal.Alloc
import Foreign.Ptr


testDecoder2 = do
    putStrLn "Test decoder file"
    ptrReg <- callocBytes 64
    ptrMem <- callocBytes 65000
    (_, codeLen) <- readCodeToPtr "data/test_1" ptrMem 0
    let ctx = Ctx (MemReg ptrReg) (MemMain ptrMem) clearFlags clearEFlags Nothing
    runPrism $ decodeMemIp decoder codeLen ctx >>= (liftIO . putStrLn . show)
    where
        decoder = makeShowDecoder


main :: IO ()
main = do
    putStrLn "Start"
    testDecoder2
    return ()
