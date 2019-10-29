{-# LANGUAGE QuasiQuotes #-}

module TestFlags where

import Test.Hspec

import Prism
import PrismCpu
import TestCommon

import NeatInterpolation

testFlagsZF env = 
    describe "Flags ZF" $ do
        it "ZF set" $ do
            code <- (assembleNative env) [text|
                mov ax, 0
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            (flagZF flags) `shouldBe` True
        it "ZF cleared" $ do
            code <- (assembleNative env) [text|
                mov al, 0
                add al, 1
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            (flagZF flags) `shouldBe` False

testFlagsCF env = 
    describe "Flags CF" $ do
        it "CF cleared" $ do
            code <- (assembleNative env) [text|
                mov al, 10
                add al, 245
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            (flagCF flags) `shouldBe` False
        it "CF cleared sub" $ do
            code <- (assembleNative env) [text|
                mov al, 0x80
                sub al, 1
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            (flagCF flags) `shouldBe` False
            al `shouldEq` 127 $ memRegN
        it "CF set" $ do
            code <- (assembleNative env) [text|
                mov al, 1
                add al, 255
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            (flagCF flags) `shouldBe` True
        it "CF set negative" $ do
            code <- (assembleNative env) [text|
                mov al, 127
                add al, -120
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            (flagCF flags) `shouldBe` True
            (flagOF flags) `shouldBe` False
        it "CF set sub" $ do
            code <- (assembleNative env) [text|
                mov al, 1
                sub al, 2
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            al `shouldEq` 255 $ memRegN
            (flagCF flags) `shouldBe` True
            (flagOF flags) `shouldBe` False
        it "CF set mul" $ do
            code <- (assembleNative env) [text|
                mov bl, 100
                mov al, 10
                mul bl
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            ax `shouldEq` 1000 $ memRegN
            (flagCF flags) `shouldBe` True
            (flagOF flags) `shouldBe` True

testFlagsOF env = 
    describe "Flags OF" $ do
        it "OF cleared" $ do
            code <- (assembleNative env) [text|
                mov al, -127
                add al, 255
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            (flagOF flags) `shouldBe` False
            (flagCF flags) `shouldBe` True
        it "OF set when ADD" $ do
            code <- (assembleNative env) [text|
                mov al, -127
                add al, 127
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            (flagOF flags) `shouldBe` False
            (flagCF flags) `shouldBe` True
        it "OF set when ADD negative" $ do
            code <- (assembleNative env) [text|
                mov al, -127
                add al, -120
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            (flagOF flags) `shouldBe` True
            (flagCF flags) `shouldBe` True
        it "OF set when SUB" $ do
            code <- (assembleNative env) [text|
                mov al, -127
                sub al, 120
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            (flagOF flags) `shouldBe` True
            (flagCF flags) `shouldBe` False
        it "OF set when SUB negative" $ do
            code <- (assembleNative env) [text|
                mov al, -127
                sub al, -120
            |]
            memRegN <- (executeNative env) code
            (flags, _) <- readFlags memRegN
            (flagOF flags) `shouldBe` False
            (flagCF flags) `shouldBe` True
