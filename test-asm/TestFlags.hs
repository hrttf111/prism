{-# LANGUAGE QuasiQuotes #-}

module TestFlags where

import Test.Hspec

import PrismCpu
import TestCommon

import NeatInterpolation

testFlagsZF execC = 
    describe "Flags ZF" $ do
        it "ZF set" $ do
            memReg <- execC [text|
                mov ax, 1
                mov cx, 2
            |]
            al `shouldEq` 1 $ memReg
            ah `shouldEq` 0 $ memReg
            ax `shouldEq` 1 $ memReg
            dl `shouldEqReg` memReg $ memReg
