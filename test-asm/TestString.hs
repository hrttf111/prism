{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module TestString where

import Test.Hspec

import Data.Text (append)

import NeatInterpolation

import Prism.Cpu
import Infra

-------------------------------------------------------------------------------

headerRep = [untrimming|
            jmp CODE_START
                str_in db 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x00
                str_in_seq db 0xDD, 0xDD, 0xDD, 0xDD, 0x00
                times 256-($-$$) db 0xAF
        CODE_START:
            mov ax, ds
            mov es, ax
            mov ax, cs
            mov ds, ax
    |]

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
                shouldEq al 0xFF
                shouldEq bl 0xAA
                shouldEq di 2
                shouldEq si 102
                shouldEqSources [al, bl]
                shouldEqSources [si, di]
                shouldEqSourcesAllFlags
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
                shouldEq al 0xFF
                shouldEq bl 0xAA
                shouldEq di 48
                shouldEq si 98
                shouldEqSources [al, bl]
                shouldEqSources [si, di]
                shouldEqSourcesAllFlags
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
                shouldEq ax 0xFFDD
                shouldEq bx 0xAAEE
                shouldEq di 4
                shouldEq si 104
                shouldEqSources [ax, bx]
                shouldEqSources [si, di]
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
                shouldEq ax 0xFFDD
                shouldEq bx 0xAAEE
                shouldEq di 46
                shouldEq si 96
                shouldEqSources [ax, bx]
                shouldEqSources [si, di]
                shouldEqSourcesAllFlags
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
                shouldEq al 1
                shouldEq bx 0
                shouldEq di 2
                shouldEq si 102
                shouldEqSources al
                shouldEqSources bx
                shouldEqSources [si, di]
                shouldEqSourcesAllFlags
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
                shouldEq bl 1
                shouldEq cl 0
                shouldEq di 2
                shouldEqSources [bl, cl]
                shouldEqSources di
                shouldEqSourcesAllFlags
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
                shouldEq al 0xAA
                shouldEq bl 0xFF
                shouldEq si 102
                shouldEqSources [al, bl]
                shouldEqSources si
                shouldEqSourcesAllFlags
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
                shouldEq al 0xFF
                shouldEq bl 0xAA
                shouldEq di 50
                shouldEqSources [al, bl]
                shouldEqSources di
                shouldEqSourcesAllFlags
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
                shouldEq al 0xFF
                shouldEq bl 0xAA
                shouldEq di 2
                shouldEq si 102
                shouldEqSources [al, bl]
                shouldEqSources [di, si]
                shouldEqSourcesAllFlags
        it "MOVS8 MEM DF=0" $ do
            runTest env (append headerRep [untrimming|
                mov di, 0
                mov si, str_in
                mov cx, 100
                rep movsb
            |]) $ do
                shouldEq di 100
                shouldEq (MemRangeDisp 0 4) $ MemRangeRes [0x11, 0x12, 0x13, 0x14, 0x15]
                shouldEq (MemDisp8 99) 0xAF
                shouldEqSources [di, si]
                shouldEqSources (MemRangeDisp 0 99)
                shouldEqSourcesAllFlags
        it "MOVS8 MEM DF=1" $ do
            runTest env (append headerRep [untrimming|
                mov di, 100
                mov si, (str_in + 99)
                mov cx, 100
                std
                rep movsb
            |]) $ do
                shouldEq di 0
                --shouldEq (MemRangeDisp 0 4) $ MemRangeRes [0x11, 0x12, 0x13, 0x14, 0x15]
                shouldEq (MemRangeDisp 1 4) $ MemRangeRes [0x11, 0x12, 0x13, 0x14]
                shouldEq (MemDisp8 99) 0xAF
                shouldEqSources [di, si]
                shouldEqSources (MemRangeDisp 0 99)
                shouldEqSourcesAllFlags
        it "MOVS16 MEM DF=0" $ do
            runTest env (append headerRep [untrimming|
                mov di, 0
                mov si, str_in
                mov cx, 50
                rep movsw
            |]) $ do
                shouldEq di 100
                shouldEq (MemRangeDisp 0 4) $ MemRangeRes [0x11, 0x12, 0x13, 0x14, 0x15]
                shouldEq (MemDisp8 99) 0xAF
                shouldEqSources [di, si]
                shouldEqSources (MemRangeDisp 0 99)
                shouldEqSourcesAllFlags
        it "MOVS16 MEM DF=1" $ do
            runTest env (append headerRep [untrimming|
                mov di, 100
                mov si, (str_in + 99)
                mov cx, 50
                std
                rep movsw
            |]) $ do
                shouldEq di 0
                --shouldEq (MemRangeDisp 0 4) $ MemRangeRes [0x11, 0x12, 0x13, 0x14, 0x15]
                shouldEq (MemRangeDisp 1 4) $ MemRangeRes [0x0, 0x12, 0x13, 0x14]
                shouldEq (MemDisp8 99) 0xAF
                shouldEqSources [di, si]
                shouldEqSources (MemRangeDisp 0 99)
                shouldEqSourcesAllFlags
        it "SCAS8 MEM DF=0" $ do
            -- Find element in string
            runTest env (append headerRep [untrimming|
                mov di, str_in
                mov cx, 255
                mov al, 0xDD
                repe scasb
            |]) $ do
                --shouldEq cx 229
                showAllRegsR
                shouldEqSources cx
                shouldEqSources di
                shouldEqSourcesAllFlags
        it "SCAS8 NE MEM DF=0" $ do
            runTest env (append headerRep [untrimming|
                mov di, str_in
                mov cx, 255
                mov al, 0xAF
                repne scasb
            |]) $ do
                --shouldEq cx 247
                showAllRegsR
                shouldEqSources cx
                shouldEqSources di
                shouldEqSourcesAllFlags
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
                shouldEq di 5
                shouldEqSources di
                shouldEqSourcesAllFlags
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
                shouldEq di 3
                shouldEqSources di
                shouldEqSourcesAllFlags
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
                shouldEq di 5
                shouldEqSources di
                shouldEqSourcesAllFlags
        it "STOS8 DF=0" $ do
            runTest env (append headerRep [untrimming|
                mov di, 0
                mov cx, 255
                mov ax, 0xBC
                repnz stosb
            |]) $ do
                shouldEqSources di
                shouldEqSourcesAllFlags
                shouldEqSources (MemRangeDisp 0 255)

-------------------------------------------------------------------------------
