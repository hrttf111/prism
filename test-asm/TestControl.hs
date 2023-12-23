{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module TestControl where

import Test.Hspec

import NeatInterpolation

import Prism.Cpu
import Infra

-------------------------------------------------------------------------------

testControl env = do
    describe "Unconditional jump" $ do
        it "jmp" $ do
            runTest env ([untrimming|
                mov al, 1
                mov bl, 0
                jmp L1
                mov bl, 1
                L1:
                mov al, 2
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesAllFlags
        it "jmp far" $ do
            runTest env ([untrimming|
                absolute 0x100
                    reg_mem    resw    1
                    seg_mem    resw    1
                section .text
                org 0
                    mov [reg_mem], WORD L1
                    mov [seg_mem], WORD cs
                    ;mov [bp], WORD reg_mem
                    mov al, 1
                    mov bl, 0
                    jmp far [reg_mem]
                    mov bl, 1
                    L1:
                    mov al, 2
                    mov al, 2
                    mov al, 2
                    mov al, 2
                    mov al, 2
                    ;hlt
            |]) $ do
                shouldEq al 2
                shouldEq bl 0
                shouldEqSourcesAllFlags
    describe "Conditional jump" $ do
        it "loop" $ do
            runTest env ([untrimming|
                mov ax, 0
                mov cx, 5
                L1:
                inc ax
                loop L1
            |]) $ do
                shouldEqSources [ax, cx]
                shouldEqSourcesAllFlags
        it "jz" $ do
            runTest env ([untrimming|
                mov al, 1
                mov bl, 0
                cmp al, 0
                jz L1
                mov bl, 1
                L1:
                mov al, 2
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesAllFlags
        it "jz 16" $ do
            runTest env ([untrimming|
                mov ax, 2
                mov bl, 0
                dec ax
                dec ax
                cmp ax, 0
                jz L1
                mov bl, 1
                L1:
                mov al, 2
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesAllFlags
        it "je" $ do
            runTest env ([untrimming|
                mov ax, 0xf111
                mov bl, 0
                LOOP1:
                cmp ax, 0
                je END
                inc bl
                ;dec ax
                ;mov ax, 0
                mov cl, 4
                shl ax, cl
                jmp LOOP1
                END:
                mov dx, 0
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesAllFlags
        it "je loop" $ do
            runTest env ([untrimming|
                mov ax, 0x1234
            NUM_LOOP:
                cmp ax, 0
                je NUM_TO_STR_END
                mov bx, ax
                and bh, 0xf0
                mov cl, 12
                shr bx, cl
                inc di
                mov cl, 4
                shl ax, cl
                jmp NUM_LOOP
            NUM_TO_STR_END:
                mov dl, 0
            |]) $ do
                shouldEqSources [ax, bx]
                shouldEqSources cl
                shouldEqSources dl
                shouldEqSourcesAllFlags
        it "jcxz" $ do
            runTest env ([untrimming|
                mov al, 1
                mov bl, 0
                mov cx, 0
                jcxz L1
                mov bl, 1
                L1:
                mov al, 2
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesAllFlags
    describe "Call" $ do
        it "near" $ do
            runTest env ([untrimming|
                mov ax, 0
                call func1
                mov dx, 12
                jmp L1
                func1:
                    mov ax, 1
                    ret
                L1:
                mov cx, 10
            |]) $ do
                shouldEqSources [ax, cx, dx]
                shouldEqSourcesAllFlags
    describe "Ret" $ do
        it "ret far" $ do
            runTest env ([untrimming|
                mov bx, L1
                push cs
                push bx
                mov al, 0
                retf
                mov al, 2
                L1:
                mov al, 1
                ;fill rest with pointless instructions
                mov cl, 0
                mov cl, 0
                mov cl, 0
                mov cl, 0
                mov cl, 0
                mov cl, 0
            |]) $ do
                shouldEq al 1
                shouldEqSources al
                shouldEqSourcesAllFlags

-------------------------------------------------------------------------------
