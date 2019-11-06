{-# LANGUAGE QuasiQuotes #-}

module TestArithmetic where

import Test.Hspec

import Prism
import PrismCpu
import Instruction.Transfer

import TestCommon

import NeatInterpolation

testAdd env = do
    describe "ADD [8] ACC REG <- IMM" $ do
        it "Simple add" $ do
            execAndCmp [al] env $ [text|
                mov al, 1
                add al, 2
            |]
        it "Add negative CF ZF" $ do
            execAndCmp [al] env $ [text|
                mov al, 1
                add al, -1
            |]
        it "Add negative CF and OF" $ do
            execAndCmp [al] env $ [text|
                mov al, -127
                add al, -120
            |]
    describe "ADD [16] ACC REG <- IMM" $ do
        it "Simple add" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 1
                add ax, 2
            |]
        it "Add negative CF ZF" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 1
                add ax, -1
            |]
        it "Add negative CF and OF" $ do
            execAndCmp [ax] env $ [text|
                mov ax, -32123
                add ax, -31234
            |]
    describe "ADD [8] REG <- REG" $ do
        it "Simple add" $ do
            execAndCmp [al, bl] env $ [text|
                mov al, 1
                mov bl, 2
                add al, bl
            |]
        it "Add negative CF and OF" $ do
            execAndCmp [al] env $ [text|
                mov al, -127
                mov bl, -120
                add bl, al
            |]
    describe "ADD [16] REG <- REG" $ do
        it "Simple add" $ do
            execAndCmp [ax, bx] env $ [text|
                mov ax, 1
                mov bx, 2
                add ax, bx
            |]
        it "Add negative CF and OF" $ do
            execAndCmp [ax] env $ [text|
                mov ax, -32123
                mov bx, -31234
                add bx, ax
            |]

testInc env = do
    describe "INC [8] REG" $ do
        it "Simple inc" $ do
            execAndCmp [al, bl, cl, dl, ah, bh, ch, dh] env $ [text|
                mov al, 1
                mov bl, 2
                mov cl, 3
                mov dl, 4
                mov ah, 5
                mov bh, 6
                mov ch, 7
                mov dh, 8
                inc al
                inc bl
                inc cl
                inc dl
                inc ah
                inc bh
                inc ch
                inc dh
            |]
        it "Simple flags" $ do
            execAndCmp [al] env $ [text|
                mov al, 0xFF
                inc al
            |]
    describe "INC [16] REG" $ do
        it "Simple inc" $ do
            execAndCmp [ax, bx, cx, dx] env $ [text|
                mov ax, 1
                mov bx, 2
                mov cx, 3
                mov dx, 4
                inc ax
                inc bx
                inc cx
                inc dx
            |]
    describe "DEC [8] REG" $ do
        it "Simple dec" $ do
            execAndCmp [al, bl, cl, dl, ah, bh, ch, dh] env $ [text|
                mov al, 1
                mov bl, 2
                mov cl, 3
                mov dl, 4
                mov ah, 5
                mov bh, 6
                mov ch, 7
                mov dh, 8
                dec al
                dec bl
                dec cl
                dec dl
                dec ah
                dec bh
                dec ch
                dec dh
            |]
        it "Simple flags" $ do
            execAndCmp [al] env $ [text|
                mov al, 0
                dec al
            |]
    describe "DEC [16] REG" $ do
        it "Simple dec" $ do
            execAndCmp [ax, bx, cx, dx] env $ [text|
                mov ax, 1
                mov bx, 2
                mov cx, 3
                mov dx, 4
                dec ax
                dec bx
                dec cx
                dec dx
            |]

testSub env = do
    describe "SUB [8] ACC REG <- IMM" $ do
        it "Simple sub" $ do
            execAndCmp [al] env $ [text|
                mov al, 2
                sub al, 1
            |]
        it "Add negative CF ZF" $ do
            execAndCmp [al] env $ [text|
                mov al, 1
                sub al, 1
            |]
        it "Add negative CF and OF" $ do
            execAndCmp [al] env $ [text|
                mov al, -127
                sub al, 120
            |]
    describe "SUB [16] ACC REG <- IMM" $ do
        it "Simple sub" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 2
                sub ax, 1
            |]
        it "Add negative CF ZF" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 1
                sub ax, 1
            |]
        it "Add negative CF and OF" $ do
            execAndCmp [ax] env $ [text|
                mov ax, -32123
                sub ax, 31234
            |]
    describe "SUB [8] REG <- REG" $ do
        it "Simple sub" $ do
            execAndCmp [al, bl] env $ [text|
                mov al, 1
                mov bl, 2
                sub al, bl
            |]
        it "Add negative CF and OF" $ do
            execAndCmp [al] env $ [text|
                mov al, -127
                mov bl, 120
                sub bl, al
            |]
    describe "SUB [16] REG <- REG" $ do
        it "Simple sub" $ do
            execAndCmp [ax, bx] env $ [text|
                mov ax, 1
                mov bx, 2
                add ax, bx
            |]
        it "Add negative CF and OF" $ do
            execAndCmp [ax] env $ [text|
                mov ax, -32123
                mov bx, 31234
                sub bx, ax
            |]

testArithOther env = do
    describe "CBW/CWD" $ do
        it "cbw no sign" $ do
            execAndCmpNF [ax] env $ [text|
                mov al, 0x7F
                cbw
            |]
        it "cbw sign" $ do
            execAndCmpNF [ax] env $ [text|
                mov al, -1
                cbw
            |]
        it "cwd no sign" $ do
            execAndCmpNF [ax, dx] env $ [text|
                mov ax, 0x7FFF
                cwd
            |]
        it "cwd sign" $ do
            execAndCmpNF [ax, dx] env $ [text|
                mov ax, -1
                cwd
            |]
    describe "NEG" $ do
        it "simple" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 1
                neg ax
            |]
        it "simple negative" $ do
            execAndCmp [ax] env $ [text|
                mov ax, -11
                neg ax
            |]
        it "CF" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 0
                neg ax
            |]
        it "OF" $ do
            execAndCmp [al] env $ [text|
                mov al, -128
                neg al
            |]
    {-describe "AAA" $ do
        it "simple" $ do
            execAndCmpNF [ax] env $ [text|
                mov al, 99
                aaa
            |]-}

testArithMuldiv env = do
    describe "MUL" $ do
        it "8 no OF/CF" $ do
            execAndCmpNF [ax] env $ [text|
                mov al, 10
                mov bl, 20
                mul bl
            |]
        it "8 OF/CF" $ do
            execAndCmpNF [ax] env $ [text|
                mov al, 100
                mov bl, 20
                mul bl
            |]
        {-it "16 no OF/CF" $ do
            execAndCmpNF [ax, dx] env $ [text|
                mov ax, 10
                mov bx, 200
                mul bx
            |]
        it "16 OF/CF" $ do
            execAndCmpNF [ax, dx] env $ [text|
                mov ax, 255
                mov bx, 300
                mul bx
            |]
            -}
    describe "IMUL" $ do
        it "8 no OF/CF" $ do
            execAndCmpNF [ax] env $ [text|
                mov al, -10
                mov bl, 20
                imul bl
            |]
        it "8 no OF/CF" $ do
            execAndCmpNF [ax] env $ [text|
                mov al, -10
                mov bl, -20
                imul bl
            |]
        it "8 OF/CF" $ do
            execAndCmpNF [ax] env $ [text|
                mov al, 100
                mov bl, -20
                imul bl
            |]
        it "8 OF/CF" $ do
            execAndCmpNF [ax] env $ [text|
                mov al, -10
                mov bl, -20
                imul bl
            |]
