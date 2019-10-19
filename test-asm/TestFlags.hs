{-# LANGUAGE QuasiQuotes #-}

module TestFlags where

import Test.Hspec

import Prism
import PrismCpu
import TestCommon

import NeatInterpolation

testFlagsZF execC = 
    describe "Flags ZF" $ do
        it "ZF set" $ do
            memReg <- execC [text|
                mov ax, 0
                pushf
                pop rax
            |]
            (flags, _) <- regToFlags memReg ax
            (flagZF flags) `shouldBe` True
        it "ZF cleared" $ do
            memReg <- execC [text|
                mov al, 0
                add al, 1
                pushf
                pop rax
            |]
            (flags, _) <- regToFlags memReg ax
            (flagZF flags) `shouldBe` False

testFlagsCF execC = 
    describe "Flags CF" $ do
        it "CF cleared" $ do
            memReg <- execC [text|
                mov al, 10
                add al, 245
                pushf
                pop rax
            |]
            (flags, _) <- regToFlags memReg ax
            (flagCF flags) `shouldBe` False
        it "CF set" $ do
            memReg <- execC [text|
                mov al, 10
                add al, 246
                pushf
                pop rax
            |]
            (flags, _) <- regToFlags memReg ax
            (flagCF flags) `shouldBe` True
