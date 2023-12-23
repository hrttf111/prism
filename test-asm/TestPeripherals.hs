{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module TestPeripherals where

import Test.Hspec

import Data.Text (Text)
import Data.IORef (readIORef, newIORef, IORef, atomicWriteIORef)
import Control.Monad.Trans (liftIO, MonadIO)

import NeatInterpolation

import Prism.Cpu
import Prism.Instructions
import Prism.Peripherals
import Infra

-------------------------------------------------------------------------------

testPeriphRead :: (Monad m, OperandVal a, Integral b) =>
    a -> b -> m a
testPeriphRead val offset = return val

testPeriphWrite :: (MonadIO m, OperandVal a, Integral b) => 
    IORef a -> b -> a -> m ()
testPeriphWrite ref offset val =
    liftIO $ atomicWriteIORef ref val >> return ()

-------------------------------------------------------------------------------

testPeripheral = do
    describe "Peripheral MMIO Remote" $ do
        it "Read 8b" $ do
            let val = 134
                handler = PeripheralHandlerMem emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                env = defaultPrismEnvPeriphMaker { prismEnvPeriphpMemRemote = [(PeripheralMem (MemLocation 9000 9200) handler)] }
            runTest env ([untrimming|
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
            let val = 0xFE19
                handler = PeripheralHandlerMem emptyWriteH emptyWriteH emptyReadH (testPeriphRead val)
                env = defaultPrismEnvPeriphMaker { prismEnvPeriphpMemRemote = [(PeripheralMem (MemLocation 8000 9200) handler)] }
            runTest env ([untrimming|
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
            let handler = PeripheralHandlerMem (testPeriphWrite ref) emptyWriteH emptyReadH emptyReadH
                env = defaultPrismEnvPeriphMaker { prismEnvPeriphpMemRemote = [(PeripheralMem (MemLocation 9000 9200) handler)] }
            runTest env ([untrimming|
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
            let handler = PeripheralHandlerMem emptyWriteH (testPeriphWrite ref) emptyReadH emptyReadH
                env = defaultPrismEnvPeriphMaker { prismEnvPeriphpMemRemote = [(PeripheralMem (MemLocation 9000 9200) handler)] }
            runTest env ([untrimming|
                xor bx, bx
                mov ds, bx
                mov [9008], WORD 0xFEAB
                hlt
            |]) $ do
                shouldEq bx 0
                liftIO $ do
                    readIORef ref >>= (`shouldBe` 0xFEAB)
    describe "Peripheral Port" $ do
        it "Read 8b" $ do
            let val = 134
                handler = PeripheralHandlerPort emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                env = defaultPrismEnvPeriphMaker { prismEnvPeriphpPortsRemote = [(PeripheralPort 120 handler)] }
            runTest env ([untrimming|
                in al, 120
                hlt
            |]) $ do
                shouldEq al 134
        it "Read 16b" $ do
            let val = 1349
                handler = PeripheralHandlerPort emptyWriteH emptyWriteH emptyReadH (testPeriphRead val)
                env = defaultPrismEnvPeriphMaker { prismEnvPeriphpPortsRemote = [(PeripheralPort 80 handler)] }
            runTest env ([untrimming|
                in ax, 80
                hlt
            |]) $ do
                shouldEq ax 1349
        it "Write 8b" $ do
            ref <- newIORef 0
            let handler = PeripheralHandlerPort (testPeriphWrite ref) emptyWriteH emptyReadH emptyReadH
                env = defaultPrismEnvPeriphMaker { prismEnvPeriphpPortsRemote = [(PeripheralPort 80 handler)] }
            runTest env ([untrimming|
                mov al, 189
                out 80, al
                hlt
            |]) $ do
                shouldEq al 189
                liftIO $ do
                    readIORef ref >>= (`shouldBe` 189)
        it "Write 16b" $ do
            ref <- newIORef 0
            let handler = PeripheralHandlerPort emptyWriteH (testPeriphWrite ref) emptyReadH emptyReadH
                env = defaultPrismEnvPeriphMaker { prismEnvPeriphpPortsRemote = [(PeripheralPort 80 handler)] }
            runTest env ([untrimming|
                mov ax, 1890
                out 80, ax
                hlt
            |]) $ do
                shouldEq ax 1890
                liftIO $ do
                    readIORef ref >>= (`shouldBe` 1890)
    describe "Peripheral MMIO Local" $ do
        it "Read 8b" $ do
            let val = 134
                handlerL = PeripheralHandlerMem emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                handlerR = PeripheralHandlerMem emptyWriteH emptyWriteH (testPeriphRead 127) emptyReadH
                env = defaultPrismEnvPeriphMaker { prismEnvPeriphpMemRemote = [(PeripheralMem (MemLocation 10300 10400) handlerR)]
                                                 , prismEnvPeriphpMemLocal = [(PeripheralMem (MemLocation 9000 9200) handlerL)]
                                                 }
            runTest env ([untrimming|
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
            let handler = PeripheralHandlerMem (testPeriphWrite ref) emptyWriteH emptyReadH emptyReadH
                env = defaultPrismEnvPeriphMaker { prismEnvPeriphpMemLocal = [(PeripheralMem (MemLocation 9000 9200) handler)] }
            runTest env ([untrimming|
                xor bx, bx
                mov ds, bx
                mov [9008], BYTE 189
                hlt
            |]) $ do
                shouldEq bx 0
                liftIO $ do
                    readIORef ref >>= (`shouldBe` 189)
    describe "Peripheral Port Local" $ do
        it "Read 8b" $ do
            let val = 134
                handlerL = PeripheralHandlerPort emptyWriteH emptyWriteH (testPeriphRead val) emptyReadH
                handlerR = PeripheralHandlerPort emptyWriteH emptyWriteH (testPeriphRead 150) emptyReadH
                env = defaultPrismEnvPeriphMaker { prismEnvPeriphpPortsRemote = [(PeripheralPort 121 handlerR)]
                                                 , prismEnvPeriphpPortsLocal = [(PeripheralPort 120 handlerL)]
                                                 }
            runTest env ([untrimming|
                in al, 121
                mov bl, al
                in al, 120
                hlt
            |]) $ do
                shouldEq al 134
                shouldEq bl 150

-------------------------------------------------------------------------------
