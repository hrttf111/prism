{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module TestString where

import Test.Hspec

import Prism.Cpu

import TestAsm.Run
import TestAsm.Common

import NeatInterpolation

-------------------------------------------------------------------------------

testString env = do
    describe "String" $ do
        it "MOVS8 DF=0" $ do
            runTest env ([untrimming|
                mov di, 0
                mov si, 100
                mov [si], BYTE 0xFF
                mov [si+1], BYTE 0xAA
                movsb
                movsb
                mov al, [es:0]
                mov bl, [es:1]
            |]) $ do
                shouldEq1 al 0xFF
                shouldEq1 bl 0xAA
                shouldEq1 di 2
                shouldEq1 si 102
        it "MOVS8 DF=1" $ do
            runTest env ([untrimming|
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
            |]) $ do
                shouldEq1 al 0xFF
                shouldEq1 bl 0xAA
                shouldEq1 di 48
                shouldEq1 si 98
        it "MOVS16 DF=0" $ do
            runTest env ([untrimming|
                mov di, 0
                mov si, 100
                mov [si], WORD 0xFFDD
                mov [si+2], WORD 0xAAEE
                movsw
                movsw
                mov ax, WORD [es:0]
                mov bx, WORD [es:2]
            |]) $ do
                shouldEq1 ax 0xFFDD
                shouldEq1 bx 0xAAEE
                shouldEq1 di 4
                shouldEq1 si 104
        it "MOVS16 DF=1" $ do
            runTest env ([untrimming|
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
            |]) $ do
                shouldEq1 ax 0xFFDD
                shouldEq1 bx 0xAAEE
                shouldEq1 di 46
                shouldEq1 si 96
        it "CMPS8 DF=0" $ do
            runTest env ([untrimming|
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
            |]) $ do
                shouldEq1 al 1
                shouldEq1 bx 0
                shouldEq1 di 2
                shouldEq1 si 102
        it "SCAS8 DF=0" $ do
            runTest env ([untrimming|
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
            |]) $ do
                shouldEq1 bl 1
                shouldEq1 cl 0
                shouldEq1 di 2
        it "lods8 DF=0" $ do
            runTest env ([untrimming|
                mov si, 100
                mov [si], BYTE 0xFF
                mov [si+1], BYTE 0xAA
                mov al, 0
                lodsb
                mov bl, al
                lodsb
            |]) $ do
                shouldEq1 al 0xAA
                shouldEq1 bl 0xFF
                shouldEq1 si 102
        it "STOS8 DF=0" $ do
            runTest env ([untrimming|
                mov di, 48
                mov al, 0xFF
                stosb
                mov al, 0xAA
                stosb
                mov bl, [es:49]
                mov al, [es:48]
            |]) $ do
                shouldEq1 al 0xFF
                shouldEq1 bl 0xAA
                shouldEq1 di 50
    describe "Rep" $ do
        it "MOVS8 DF=0" $ do
            runTest env ([untrimming|
                mov di, 0
                mov si, 100
                mov [si], BYTE 0xFF
                mov [si+1], BYTE 0xAA
                mov cx, 2
                rep movsb
                mov al, [es:0]
                mov bl, [es:1]
            |]) $ do
                shouldEq1 al 0xFF
                shouldEq1 bl 0xAA
                shouldEq1 di 2
                shouldEq1 si 102
        it "SCAS8 NE" $ do
            runTest env ([untrimming|
                mov di, 0
                mov [di], BYTE 0xFF
                mov [di+1], BYTE 0xAB
                mov [di+2], BYTE 0xAC
                mov [di+3], BYTE 0xAF
                mov [di+4], BYTE 0
                mov cx, 5
                mov al, 0xDD
                repne scasb
            |]) $ do
                shouldEq1 di 5
        it "SCAS8 NE found" $ do
            runTest env ([untrimming|
                mov di, 0
                mov [es:di], BYTE 0xFF
                mov [es:di+1], BYTE 0xAB
                mov [es:di+2], BYTE 0xDD
                mov [es:di+3], BYTE 0xAF
                mov [es:di+4], BYTE 0
                mov cx, 5
                mov al, 0xDD
                repne scasb
            |]) $ do
                shouldEq1 di 3
        it "SCAS8 NZ found" $ do
            runTest env ([untrimming|
                mov di, 0
                mov [es:di], BYTE 0xFF
                mov [es:di+1], BYTE 0xAB
                mov [es:di+2], BYTE 0xDD
                mov [es:di+3], BYTE 0xAF
                mov [es:di+4], BYTE 0
                mov cx, 10
                mov ax, 0
                repnz scasb
            |]) $ do
                shouldEq1 di 5

-------------------------------------------------------------------------------
