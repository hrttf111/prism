{-# LANGUAGE QuasiQuotes #-}

module TestPeripherals where

import Test.Hspec

import Prism
import PrismCpu
import PrismPeripheral

import TestCommon

import NeatInterpolation


testPeripheral env = do
    describe "Peripheral MMIO" $ do
        it "Read 8b" $ do
            execAndCmp [al] env $ [text|
                mov al, 1
                add al, 2
            |]
