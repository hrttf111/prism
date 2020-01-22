{-# LANGUAGE QuasiQuotes #-}

module TestPeripherals where

import Test.Hspec

import Data.IORef
import Control.Concurrent

import Prism
import PrismCpu
import PrismPeripheral

import TestCommon

import NeatInterpolation

-------------------------------------------------------------------------------

testPeriphRead :: (OperandVal a, Integral b) =>
    a -> Peripheral -> b -> IO (Peripheral, a)
testPeriphRead val peripheral offset = return (peripheral, val)

testPeriphWrite :: (OperandVal a, Integral b) => 
    IORef a -> Peripheral -> b -> a -> IO Peripheral
testPeriphWrite ref peripheral offset val =
    atomicWriteIORef ref val >> return peripheral

-------------------------------------------------------------------------------

testPeripheral instrList = do
    describe "Peripheral MMIO" $ do
        it "Read 8b" $ do
            let devices = PeripheralDevices
                val = 134
                handler = PeripheralHandlerMem emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                mem = [(PeripheralMem (9000, 9200) handler)]
            env <- createPeripheralsTestEnv instrList devices [] mem
            execPrism [(al `shouldEq` 134), (bl `shouldEq` 189)] env $ [text|
                xor bx, bx
                mov ds, bx
                mov [8999], BYTE 189
                mov al, [9002]
                mov bl, [8999]
            |]
        it "Read 16b" $ do
            let devices = PeripheralDevices
                val = 0xFE19
                handler = PeripheralHandlerMem emptyWriteH emptyWriteH emptyReadH (testPeriphRead val)
                mem = [(PeripheralMem (8000, 9200) handler)]
            env <- createPeripheralsTestEnv instrList devices [] mem
            execPrism [(ax `shouldEq` 0xFE19), (bx `shouldEq` 1089)] env $ [text|
                xor bx, bx
                mov ds, bx
                mov [7998], WORD 1089
                mov ax, [9002]
                mov bx, [7998]
            |]
        it "Write 8b" $ do
            ref <- newIORef 0
            let devices = PeripheralDevices
                handler = PeripheralHandlerMem (testPeriphWrite ref) emptyWriteH emptyReadH emptyReadH
                mem = [(PeripheralMem (9000, 9200) handler)]
            env <- createPeripheralsTestEnv instrList devices [] mem
            execPrism [] env $ [text|
                xor bx, bx
                mov ds, bx
                mov [9008], BYTE 189
            |]
            threadDelay 100
            readIORef ref >>= (`shouldBe` 189)
        it "Write 16b" $ do
            ref <- newIORef 0
            let devices = PeripheralDevices
                handler = PeripheralHandlerMem emptyWriteH (testPeriphWrite ref) emptyReadH emptyReadH
                mem = [(PeripheralMem (9000, 9200) handler)]
            env <- createPeripheralsTestEnv instrList devices [] mem
            execPrism [] env $ [text|
                xor bx, bx
                mov ds, bx
                mov [9008], WORD 0xFEAB
            |]
            threadDelay 100
            readIORef ref >>= (`shouldBe` 0xFEAB)
    describe "Peripheral Port" $ do
        it "Read 8b" $ do
            let devices = PeripheralDevices
                val = 134
                handler = PeripheralHandlerPort emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                port = [(PeripheralPort 120 handler)]
            env <- createPeripheralsTestEnv instrList devices port []
            execPrism [(al `shouldEq` 134)] env $ [text|
                in al, 120
            |]
        it "Read 16b" $ do
            let devices = PeripheralDevices
                val = 1349
                handler = PeripheralHandlerPort emptyWriteH emptyWriteH emptyReadH (testPeriphRead val)
                port = [(PeripheralPort 80 handler)]
            env <- createPeripheralsTestEnv instrList devices port []
            execPrism [(ax `shouldEq` 1349)] env $ [text|
                in ax, 80
            |]
        it "Write 8b" $ do
            ref <- newIORef 0
            let devices = PeripheralDevices
                handler = PeripheralHandlerPort (testPeriphWrite ref) emptyWriteH emptyReadH emptyReadH
                port = [(PeripheralPort 80 handler)]
            env <- createPeripheralsTestEnv instrList devices port []
            execPrism [] env $ [text|
                mov al, 189
                out 80, al
            |]
            threadDelay 10000
            readIORef ref >>= (`shouldBe` 189)
        it "Write 16b" $ do
            ref <- newIORef 0
            let devices = PeripheralDevices
                handler = PeripheralHandlerPort emptyWriteH (testPeriphWrite ref) emptyReadH emptyReadH
                port = [(PeripheralPort 80 handler)]
            env <- createPeripheralsTestEnv instrList devices port []
            execPrism [] env $ [text|
                mov ax, 1890
                out 80, ax
            |]
            threadDelay 10000
            readIORef ref >>= (`shouldBe` 1890)

-------------------------------------------------------------------------------
