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
            |]
            (flags, _) <- readFlags memReg
            (flagZF flags) `shouldBe` True
        it "ZF cleared" $ do
            memReg <- execC [text|
                mov al, 0
                add al, 1
            |]
            (flags, _) <- readFlags memReg
            (flagZF flags) `shouldBe` False

testFlagsCF execC = 
    describe "Flags CF" $ do
        it "CF cleared" $ do
            memReg <- execC [text|
                mov al, 10
                add al, 245
            |]
            (flags, _) <- readFlags memReg
            (flagCF flags) `shouldBe` False
        it "CF cleared sub" $ do
            memReg <- execC [text|
                mov al, 0x80
                sub al, 1
            |]
            (flags, _) <- readFlags memReg
            (flagCF flags) `shouldBe` False
            al `shouldEq` 127 $ memReg
        it "CF set" $ do
            memReg <- execC [text|
                mov al, 1
                add al, 255
            |]
            (flags, _) <- readFlags memReg
            (flagCF flags) `shouldBe` True
        it "CF set negative" $ do
            memReg <- execC [text|
                mov al, 127
                add al, -120
            |]
            (flags, _) <- readFlags memReg
            (flagCF flags) `shouldBe` True
            (flagOF flags) `shouldBe` False
        it "CF set sub" $ do
            memReg <- execC [text|
                mov al, 1
                sub al, 2
            |]
            (flags, _) <- readFlags memReg
            al `shouldEq` 255 $ memReg
            (flagCF flags) `shouldBe` True
            (flagOF flags) `shouldBe` False

testFlagsOF execC = 
    describe "Flags OF" $ do
        it "OF cleared" $ do
            memReg <- execC [text|
                mov al, -127
                add al, 255
            |]
            (flags, _) <- readFlags memReg
            (flagOF flags) `shouldBe` False
            (flagCF flags) `shouldBe` True
        it "OF set when ADD" $ do
            memReg <- execC [text|
                mov al, -127
                add al, 127
            |]
            (flags, _) <- readFlags memReg
            (flagOF flags) `shouldBe` False
            (flagCF flags) `shouldBe` True
        it "OF set when ADD negative" $ do
            memReg <- execC [text|
                mov al, -127
                add al, -120
            |]
            (flags, _) <- readFlags memReg
            (flagOF flags) `shouldBe` True
            (flagCF flags) `shouldBe` True
        it "OF set when SUB" $ do
            memReg <- execC [text|
                mov al, -127
                sub al, 120
            |]
            (flags, _) <- readFlags memReg
            (flagOF flags) `shouldBe` True
            (flagCF flags) `shouldBe` False
        it "OF set when SUB negative" $ do
            memReg <- execC [text|
                mov al, -127
                sub al, -120
            |]
            (flags, _) <- readFlags memReg
            (flagOF flags) `shouldBe` False
            (flagCF flags) `shouldBe` True
