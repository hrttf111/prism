{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module TestPeripherals where

import Test.Hspec

import Data.Text (Text)
import Data.IORef (readIORef, newIORef, IORef, atomicWriteIORef)
import Control.Monad.Trans (liftIO, MonadIO)

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

preStartAction :: PrismM ()
preStartAction = return ()

testPeripheral = do
    describe "Peripheral MMIO Remote" $ do
        let devL = TestDev
        it "Read 8b" $ do
            let devices = TestDev
                val = 134
                handler = PeripheralHandlerMem emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                mem = [(PeripheralMem (MemLocation 9000 9200) handler)]
            env <- makePrismPeripheralsEnv devices [] mem devL [] [] [] preStartAction
            execTestEnvIO env ([untrimming|
                xor bx, bx
                mov ds, bx
                mov [8999], BYTE 189
                mov al, [9002]
                mov bl, [8999]
                hlt
            |]) $ do
                shouldEq al 134
                shouldEq bl 189
        it "Read 16b" $ do
            let devices = TestDev
                val = 0xFE19
                handler = PeripheralHandlerMem emptyWriteH emptyWriteH emptyReadH (testPeriphRead val)
                mem = [(PeripheralMem (MemLocation 8000 9200) handler)]
            env <- makePrismPeripheralsEnv devices [] mem devL [] [] [] preStartAction
            execTestEnvIO env ([untrimming|
                xor bx, bx
                mov ds, bx
                mov [7998], WORD 1089
                mov ax, [9002]
                mov bx, [7998]
                hlt
            |]) $ do
                shouldEq ax 0xFE19
                shouldEq bx 1089
        it "Write 8b" $ do
            ref <- newIORef 0
            let devices = TestDev
                handler = PeripheralHandlerMem (testPeriphWrite ref) emptyWriteH emptyReadH emptyReadH
                mem = [(PeripheralMem (MemLocation 9000 9200) handler)]
            env <- makePrismPeripheralsEnv devices [] mem devL [] [] [] preStartAction
            execTestEnvIO env ([untrimming|
                xor bx, bx
                mov ds, bx
                mov [9008], BYTE 189
                hlt
            |]) $ do
                shouldEq bx 0
                liftIO $ do
                    readIORef ref >>= (`shouldBe` 189)
        it "Write 16b" $ do
            ref <- newIORef 0
            let devices = TestDev
                handler = PeripheralHandlerMem emptyWriteH (testPeriphWrite ref) emptyReadH emptyReadH
                mem = [(PeripheralMem (MemLocation 9000 9200) handler)]
            env <- makePrismPeripheralsEnv devices [] mem devL [] [] [] preStartAction
            execTestEnvIO env ([untrimming|
                xor bx, bx
                mov ds, bx
                mov [9008], WORD 0xFEAB
                hlt
            |]) $ do
                shouldEq bx 0
                liftIO $ do
                    readIORef ref >>= (`shouldBe` 0xFEAB)
    describe "Peripheral Port" $ do
        let devL = TestDev
        it "Read 8b" $ do
            let devices = TestDev
                val = 134
                handler = PeripheralHandlerPort emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                port = [(PeripheralPort 120 handler)]
            env <- makePrismPeripheralsEnv devices port [] devL [] [] [] preStartAction
            execTestEnvIO env ([untrimming|
                in al, 120
                hlt
            |]) $ do
                shouldEq al 134
        it "Read 16b" $ do
            let devices = TestDev
                val = 1349
                handler = PeripheralHandlerPort emptyWriteH emptyWriteH emptyReadH (testPeriphRead val)
                port = [(PeripheralPort 80 handler)]
            env <- makePrismPeripheralsEnv devices port [] devL [] [] [] preStartAction
            execTestEnvIO env ([untrimming|
                in ax, 80
                hlt
            |]) $ do
                shouldEq ax 1349
        it "Write 8b" $ do
            ref <- newIORef 0
            let devices = TestDev
                handler = PeripheralHandlerPort (testPeriphWrite ref) emptyWriteH emptyReadH emptyReadH
                port = [(PeripheralPort 80 handler)]
            env <- makePrismPeripheralsEnv devices port [] devL [] [] [] preStartAction
            execTestEnvIO env ([untrimming|
                mov al, 189
                out 80, al
                hlt
            |]) $ do
                shouldEq al 189
                liftIO $ do
                    readIORef ref >>= (`shouldBe` 189)
        it "Write 16b" $ do
            ref <- newIORef 0
            let devices = TestDev
                handler = PeripheralHandlerPort emptyWriteH (testPeriphWrite ref) emptyReadH emptyReadH
                port = [(PeripheralPort 80 handler)]
            env <- makePrismPeripheralsEnv devices port [] devL [] [] [] preStartAction
            execTestEnvIO env ([untrimming|
                mov ax, 1890
                out 80, ax
                hlt
            |]) $ do
                shouldEq ax 1890
                liftIO $ do
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
            env <- makePrismPeripheralsEnv devR [] memR devices [] memL [] preStartAction
            execTestEnvIO env ([untrimming|
                xor bx, bx
                mov ds, bx
                mov [8999], BYTE 189
                mov al, [9002]
                mov bl, [8999]
                mov cl, [10300]
                hlt
            |]) $ do
                shouldEq al 134
                shouldEq bl 189
                shouldEq cl 127
        it "Write 8b" $ do
            ref <- newIORef 0
            let devices = TestDev
                handler = PeripheralHandlerMem (testPeriphWrite ref) emptyWriteH emptyReadH emptyReadH
                memL = [(PeripheralMem (MemLocation 9000 9200) handler)]
                memR = []
            env <- makePrismPeripheralsEnv devR [] memR devices [] memL [] preStartAction
            execTestEnvIO env ([untrimming|
                xor bx, bx
                mov ds, bx
                mov [9008], BYTE 189
                hlt
            |]) $ do
                shouldEq bx 0
                liftIO $ do
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
            env <- makePrismPeripheralsEnv devR portR [] devices portL [] [] preStartAction
            execTestEnvIO env ([untrimming|
                in al, 121
                mov bl, al
                in al, 120
                hlt
            |]) $ do
                shouldEq al 134
                shouldEq bl 150

-------------------------------------------------------------------------------
