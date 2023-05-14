{-# LANGUAGE QuasiQuotes #-}

module TestControl where

import Test.Hspec

import Prism.Cpu

import TestAsm.Run
import TestAsm.Common

import NeatInterpolation

-------------------------------------------------------------------------------

testControl env = do
    describe "Unconditional jump" $ do
        it "jmp" $ do
            execAndCmpNF [al, bl] env $ [text|
                mov al, 1
                mov bl, 0
                jmp L1
                mov bl, 1
                L1:
                mov al, 2
            |]
    describe "Conditional jump" $ do
        it "loop" $ do
            execAndCmpNF [ax, cx] env $ [text|
                mov ax, 0
                mov cx, 5
                L1:
                inc ax
                loop L1
            |]
        it "jz" $ do
            execAndCmpNF [al, bl] env $ [text|
                mov al, 1
                mov bl, 0
                cmp al, 0
                jz L1
                mov bl, 1
                L1:
                mov al, 2
            |]
        it "jz 16" $ do
            execAndCmpNF [al, bl] env $ [text|
                mov ax, 2
                mov bl, 0
                dec ax
                dec ax
                cmp ax, 0
                jz L1
                mov bl, 1
                L1:
                mov al, 2
            |]
        it "je" $ do
            execAndCmpNF [al, bl] env $ [text|
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
            |]
        it "je loop" $ do
            execAndCmpNF [ax, bx, dx, cx] env $ [text|
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
            |]
        it "jcxz" $ do
            execAndCmpNF [al, bl] env $ [text|
                mov al, 1
                mov bl, 0
                mov cx, 0
                jcxz L1
                mov bl, 1
                L1:
                mov al, 2
            |]
    describe "Call" $ do
        it "near" $ do
            execAndCmpNF [ax, cx, dx] env $ [text|
                mov ax, 0
                call func1
                mov dx, 12
                jmp L1
                func1:
                    mov ax, 1
                    ret
                L1:
                mov cx, 10
            |]

-------------------------------------------------------------------------------
