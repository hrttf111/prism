{-# LANGUAGE QuasiQuotes #-}

module TestString where

import Test.Hspec

import Prism
import PrismCpu

import TestCommon

import NeatInterpolation

testString env = do
    describe "String" $ do
        it "MOVS8 DF=0" $ do
            execPrism [(al `shouldEq` 0xFF), (bl `shouldEq` 0xAA), (di `shouldEq` 2), (si `shouldEq` 102)] env [text|
                mov di, 0
                mov si, 100
                mov [si], BYTE 0xFF
                mov [si+1], BYTE 0xAA
                movsb
                movsb
                mov al, [es:0]
                mov bl, [es:1]
            |]
        it "MOVS8 DF=1" $ do
            execPrism [(al `shouldEq` 0xFF), (bl `shouldEq` 0xAA), (di `shouldEq` 48), (si `shouldEq` 98)] env [text|
                mov di, 50
                mov si, 99
                mov [si], BYTE 0xFF
                mov [si+1], BYTE 0xAA
                mov si, 100
                std
                movsb
                movsb
                mov bl, [es:50]
                mov al, [es:49]
            |]
        it "MOVS16 DF=0" $ do
            execPrism [(ax `shouldEq` 0xFFDD), (bx `shouldEq` 0xAAEE), (di `shouldEq` 4), (si `shouldEq` 104)] env [text|
                mov di, 0
                mov si, 100
                mov [si], WORD 0xFFDD
                mov [si+2], WORD 0xAAEE
                movsw
                movsw
                mov ax, WORD [es:0]
                mov bx, WORD [es:2]
            |]
        it "MOVS16 DF=1" $ do
            execPrism [(ax `shouldEq` 0xFFDD), (bx `shouldEq` 0xAAEE), (di `shouldEq` 46), (si `shouldEq` 96)] env [text|
                mov di, 50
                mov si, 98
                mov [si], WORD 0xFFDD
                mov [si+2], WORD 0xAAEE
                mov si, 100
                std
                movsw
                movsw
                mov bx, WORD [es:50]
                mov ax, WORD [es:48]
            |]
        it "CMPS8 DF=0" $ do
            execPrism [(al `shouldEq` 1), (bl `shouldEq` 0), (di `shouldEq` 2), (si `shouldEq` 102)] env [text|
                mov di, 0
                mov si, 100
                mov [si], BYTE 0xFF
                mov [si+1], BYTE 0xAA
                mov [es:di], BYTE 0xFF
                mov [es:di+1], BYTE 0xAB
                cmpsb
                mov al, 0
                jnz NOT_EQ
                mov al, 1
                NOT_EQ:
                cmpsb
                mov bl, 1
                jz EQ
                mov bl, 0
                EQ:
            |]
        it "SCAS8 DF=0" $ do
            execPrism [(bl `shouldEq` 1), (cl `shouldEq` 0), (di `shouldEq` 2)] env [text|
                mov di, 0
                mov [es:di], BYTE 0xFF
                mov [es:di+1], BYTE 0xAB
                mov al, 0xFF
                scasb
                mov bl, 0
                jnz NOT_EQ
                mov bl, 1
                NOT_EQ:
                scasb
                mov cl, 1
                jz EQ
                mov cl, 0
                EQ:
            |]
        it "lods8 DF=0" $ do
            execPrism [(al `shouldEq` 0xAA), (bl `shouldEq` 0xFF), (si `shouldEq` 102)] env [text|
                mov si, 100
                mov [si], BYTE 0xFF
                mov [si+1], BYTE 0xAA
                mov al, 0
                lodsb
                mov bl, al
                lodsb
            |]
        it "STOS8 DF=0" $ do
            execPrism [(al `shouldEq` 0xFF), (bl `shouldEq` 0xAA), (di `shouldEq` 50)] env [text|
                mov di, 48
                mov al, 0xFF
                stosb
                mov al, 0xAA
                stosb
                mov bl, [es:49]
                mov al, [es:48]
            |]
