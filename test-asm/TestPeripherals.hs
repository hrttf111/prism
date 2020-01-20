{-# LANGUAGE QuasiQuotes #-}

module TestPeripherals where

import Test.Hspec

import Prism
import PrismCpu
import PrismPeripheral

import TestCommon

import NeatInterpolation

-------------------------------------------------------------------------------

testMmioRead8 :: Uint8 -> Peripheral -> MemOffset -> IO (Peripheral, Uint8)
testMmioRead8 val peripheral offset = return (peripheral, val)

testMmioRead16 :: Uint16 -> Peripheral -> MemOffset -> IO (Peripheral, Uint16)
testMmioRead16 val peripheral offset = return (peripheral, val)

-------------------------------------------------------------------------------

testPeripheral instrList = do
    describe "Peripheral MMIO" $ do
        it "Read 8b" $ do
            let devices = PeripheralDevices
                val = 134
                handler = PeripheralHandlerMem emptyWriteH emptyWriteH (testMmioRead8 val) emptyReadH
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
                handler = PeripheralHandlerMem emptyWriteH emptyWriteH emptyReadH (testMmioRead16 val)
                mem = [(PeripheralMem (8000, 9200) handler)]
            env <- createPeripheralsTestEnv instrList devices [] mem
            execPrism [(ax `shouldEq` 0xFE19), (bx `shouldEq` 1089)] env $ [text|
                xor bx, bx
                mov ds, bx
                mov [7998], WORD 1089
                mov ax, [9002]
                mov bx, [7998]
            |]

-------------------------------------------------------------------------------
