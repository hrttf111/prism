{-# LANGUAGE QuasiQuotes #-}

module TestPeripherals where

import Test.Hspec

import Data.IORef
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Concurrent

import Prism.Cpu
import Prism.Instructions
import Prism.Peripherals

import TestAsm.Run
import TestAsm.Common

import NeatInterpolation

-------------------------------------------------------------------------------

testPeriphRead :: (Monad m, OperandVal a, Integral b) =>
    a -> b -> m a
testPeriphRead val offset = return val

testPeriphWrite :: (MonadIO m, OperandVal a, Integral b) => 
    IORef a -> b -> a -> m ()
testPeriphWrite ref offset val =
    liftIO $ atomicWriteIORef ref val >> return ()

-------------------------------------------------------------------------------

testPeripheral instrList = do
    describe "Peripheral MMIO Remote" $ do
        let devL = TestDev
        it "Read 8b" $ do
            let devices = TestDev
                val = 134
                handler = PeripheralHandlerMem emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                mem = [(PeripheralMem (MemLocation 9000 9200) handler)]
            env <- createPeripheralsTestEnv instrList devices [] mem devL [] [] []
            execPrism [(al `shouldEq` 134), (bl `shouldEq` 189)] env $ [text|
                xor bx, bx
                mov ds, bx
                mov [8999], BYTE 189
                mov al, [9002]
                mov bl, [8999]
            |]
        it "Read 16b" $ do
            let devices = TestDev
                val = 0xFE19
                handler = PeripheralHandlerMem emptyWriteH emptyWriteH emptyReadH (testPeriphRead val)
                mem = [(PeripheralMem (MemLocation 8000 9200) handler)]
            env <- createPeripheralsTestEnv instrList devices [] mem devL [] [] []
            execPrism [(ax `shouldEq` 0xFE19), (bx `shouldEq` 1089)] env $ [text|
                xor bx, bx
                mov ds, bx
                mov [7998], WORD 1089
                mov ax, [9002]
                mov bx, [7998]
            |]
        it "Write 8b" $ do
            ref <- newIORef 0
            let devices = TestDev
                handler = PeripheralHandlerMem (testPeriphWrite ref) emptyWriteH emptyReadH emptyReadH
                mem = [(PeripheralMem (MemLocation 9000 9200) handler)]
            env <- createPeripheralsTestEnv instrList devices [] mem devL [] [] []
            execPrism [] env $ [text|
                xor bx, bx
                mov ds, bx
                mov [9008], BYTE 189
            |]
            threadDelay 100
            readIORef ref >>= (`shouldBe` 189)
        it "Write 16b" $ do
            ref <- newIORef 0
            let devices = TestDev
                handler = PeripheralHandlerMem emptyWriteH (testPeriphWrite ref) emptyReadH emptyReadH
                mem = [(PeripheralMem (MemLocation 9000 9200) handler)]
            env <- createPeripheralsTestEnv instrList devices [] mem devL [] [] []
            execPrism [] env $ [text|
                xor bx, bx
                mov ds, bx
                mov [9008], WORD 0xFEAB
            |]
            threadDelay 100
            readIORef ref >>= (`shouldBe` 0xFEAB)
    describe "Peripheral Port" $ do
        let devL = TestDev
        it "Read 8b" $ do
            let devices = TestDev
                val = 134
                handler = PeripheralHandlerPort emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                port = [(PeripheralPort 120 handler)]
            env <- createPeripheralsTestEnv instrList devices port [] devL [] [] []
            execPrism [(al `shouldEq` 134)] env $ [text|
                in al, 120
            |]
        it "Read 16b" $ do
            let devices = TestDev
                val = 1349
                handler = PeripheralHandlerPort emptyWriteH emptyWriteH emptyReadH (testPeriphRead val)
                port = [(PeripheralPort 80 handler)]
            env <- createPeripheralsTestEnv instrList devices port [] devL [] [] []
            execPrism [(ax `shouldEq` 1349)] env $ [text|
                in ax, 80
            |]
        it "Write 8b" $ do
            ref <- newIORef 0
            let devices = TestDev
                handler = PeripheralHandlerPort (testPeriphWrite ref) emptyWriteH emptyReadH emptyReadH
                port = [(PeripheralPort 80 handler)]
            env <- createPeripheralsTestEnv instrList devices port [] devL [] [] []
            execPrism [] env $ [text|
                mov al, 189
                out 80, al
            |]
            threadDelay 10000
            readIORef ref >>= (`shouldBe` 189)
        it "Write 16b" $ do
            ref <- newIORef 0
            let devices = TestDev
                handler = PeripheralHandlerPort emptyWriteH (testPeriphWrite ref) emptyReadH emptyReadH
                port = [(PeripheralPort 80 handler)]
            env <- createPeripheralsTestEnv instrList devices port [] devL [] [] []
            execPrism [] env $ [text|
                mov ax, 1890
                out 80, ax
            |]
            threadDelay 10000
            readIORef ref >>= (`shouldBe` 1890)
    describe "Peripheral MMIO Local" $ do
        let devR = TestDev
        it "Read 8b" $ do
            let devices = TestDev
                val = 134
                handlerL = PeripheralHandlerMem emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                memL = [(PeripheralMem (MemLocation 9000 9200) handlerL)]
                handlerR = PeripheralHandlerMem emptyWriteH emptyWriteH (testPeriphRead 127) emptyReadH
                memR = [(PeripheralMem (MemLocation 10300 10400) handlerR)]
            env <- createPeripheralsTestEnv instrList devR [] memR devices [] memL []
            execPrism [(al `shouldEq` 134), (bl `shouldEq` 189), (cl `shouldEq` 127)] env $ [text|
                xor bx, bx
                mov ds, bx
                mov [8999], BYTE 189
                mov al, [9002]
                mov bl, [8999]
                mov cl, [10300]
            |]
        it "Write 8b" $ do
            ref <- newIORef 0
            let devices = TestDev
                handler = PeripheralHandlerMem (testPeriphWrite ref) emptyWriteH emptyReadH emptyReadH
                memL = [(PeripheralMem (MemLocation 9000 9200) handler)]
                memR = []
            env <- createPeripheralsTestEnv instrList devR [] memR devices [] memL []
            execPrism [] env $ [text|
                xor bx, bx
                mov ds, bx
                mov [9008], BYTE 189
            |]
            threadDelay 100
            readIORef ref >>= (`shouldBe` 189)
    describe "Peripheral Port Local" $ do
        let devR = TestDev
        it "Read 8b" $ do
            let devices = TestDev
                val = 134
                handlerL = PeripheralHandlerPort emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                portL = [(PeripheralPort 120 handlerL)]
                handlerR = PeripheralHandlerPort emptyWriteH emptyWriteH (testPeriphRead 150) emptyReadH
                portR = [(PeripheralPort 121 handlerR)]
            env <- createPeripheralsTestEnv instrList devR portR [] devices portL [] []
            execPrism [(al `shouldEq` 134), (bl `shouldEq` 150)] env $ [text|
                in al, 121
                mov bl, al
                in al, 120
            |]

-------------------------------------------------------------------------------
