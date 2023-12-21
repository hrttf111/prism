{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module TestArithmetic where

import Test.Hspec

import Prism.Cpu

import TestAsm.Run
import TestAsm.Common

import NeatInterpolation

-------------------------------------------------------------------------------
testAdd env = do
    describe "ADD [8] ACC REG <- IMM" $ do
        it "Simple add" $ do
            runTest env ([untrimming|
                mov al, 1
                add al, 2
            |]) $ do
                shouldEq al 3
                shouldEqSources al
                shouldEqSourcesAllFlags
        it "Add negative CF ZF" $ do
            runTest env ([untrimming|
                mov al, 1
                add al, -1
            |]) $ do
                shouldEq al 0
                shouldEq [CF, ZF] [True, True]
                shouldEqSources al
                shouldEqSourcesAllFlags
        it "Add negative CF and OF" $ do
            runTest env ([untrimming|
                mov al, -127
                add al, -120
            |]) $ do
                shouldEq [CF, OF] [True, True]
                shouldEqSources al
                shouldEqSourcesAllFlags
    describe "ADD [16] ACC REG <- IMM" $ do
        it "Simple add" $ do
            runTest env ([untrimming|
                mov ax, 1
                add ax, 2
            |]) $ do
                shouldEq ax 3
                shouldEqSources ax
                shouldEqSourcesAllFlags
        it "Add negative CF ZF" $ do
            runTest env ([untrimming|
                mov ax, 1
                add ax, -1
            |]) $ do
                shouldEq ax 0
                shouldEq [CF, ZF] [True, True]
                shouldEqSources ax
                shouldEqSourcesAllFlags
        it "Add negative CF and OF" $ do
            runTest env ([untrimming|
                mov ax, -32123
                add ax, -31234
            |]) $ do
                shouldEq [CF, OF] [True, True]
                shouldEqSources ax
                shouldEqSourcesAllFlags
        it "Add negative CF and OF 2" $ do
            runTest env ([untrimming|
                mov ax, 0x2002
                add ax, -100
            |]) $ do
                shouldEq ax $ 0x2002 - 100
                shouldEqSources ax
                shouldEqSourcesAllFlags
            runTest env ([untrimming|
                mov ax, 0x2002
                add ax, -100
            |]) $ do
                shouldEq ax $ 0x2002 - 100
                shouldEqSources ax
                shouldEqSourcesAllFlags
    describe "ADD [8] REG <- REG" $ do
        it "Simple add" $ do
            runTest env ([untrimming|
                mov al, 1
                mov bl, 2
                add al, bl
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesAllFlags
        it "Add negative CF and OF" $ do
            runTest env ([untrimming|
                mov al, -127
                mov bl, -120
                add bl, al
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesAllFlags
    describe "ADD [16] REG <- REG" $ do
        it "Simple add" $ do
            runTest env ([untrimming|
                mov ax, 1
                mov bx, 2
                add ax, bx
            |]) $ do
                shouldEqSources [ax, bx]
                shouldEqSourcesAllFlags
        it "Simple add1" $ do
            runTest env ([untrimming|
                mov ax, 0xffff
                mov bx, 2
                add ax, bx
            |]) $ do
                shouldEqSources [ax, bx]
                shouldEqSourcesAllFlags
        it "Simple add2" $ do
            runTest env ([untrimming|
                mov ax, 2
                mov bx, 0xffff
                add ax, bx
            |]) $ do
                shouldEqSources [ax, bx]
                shouldEqSourcesAllFlags
        it "Add negative CF and OF" $ do
            runTest env ([untrimming|
                mov ax, -32123
                mov bx, -31234
                add bx, ax
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesAllFlags
    describe "ADC [16] REG <- REG" $ do
        it "ADC with CF" $ do
            runTest env ([untrimming|
                mov bx, 0xFFFF
                adc bx, bx
            |]) $ do
                shouldEqSources bx
                shouldEqSourcesAllFlags
        it "ADC with CF2" $ do
            runTest env ([untrimming|
                mov bx, 0xFFFF
                adc bx, bx
                mov bx, 0xFFFF
                adc bx, bx
            |]) $ do
                shouldEqSources bx
                shouldEqSourcesAllFlags

testInc env = do
    describe "INC [8] REG" $ do
        it "Simple inc" $ do
            runTest env ([untrimming|
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
            |]) $ do
                shouldEqSources [al, bl, cl, dl, ah, bh, ch, dh]
                shouldEqSourcesAllFlags
        it "Simple flags" $ do
            runTest env ([untrimming|
                mov al, 0xFF
                inc al
            |]) $ do
                shouldEqSources al
                shouldEqSourcesAllFlags
    describe "INC [16] REG" $ do
        it "Simple inc" $ do
            runTest env ([untrimming|
                mov ax, 1
                mov bx, 2
                mov cx, 3
                mov dx, 4
                inc ax
                inc bx
                inc cx
                inc dx
            |]) $ do
                shouldEqSources [ax, bx, cx, dx]
                shouldEqSourcesAllFlags
    describe "DEC [8] REG" $ do
        it "Simple dec" $ do
            runTest env ([untrimming|
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
            |]) $ do
                shouldEqSources [al, bl, cl, dl, ah, bh, ch, dh]
                shouldEqSourcesAllFlags
        it "Simple flags" $ do
            runTest env ([untrimming|
                mov al, 0
                dec al
            |]) $ do
                shouldEqSources al
                shouldEqSourcesAllFlags
    describe "DEC [16] REG" $ do
        it "Simple dec" $ do
            runTest env ([untrimming|
                mov ax, 1
                mov bx, 2
                mov cx, 3
                mov dx, 4
                dec ax
                dec bx
                dec cx
                dec dx
            |]) $ do
                shouldEqSources [ax, bx, cx, dx]
                shouldEqSourcesAllFlags
        it "Simple dec ax" $ do
            runTest env ([untrimming|
                mov ax, 5
                mov bx, 2
                mov cx, 3
                mov dx, 4
                dec ax
                dec ax
                dec ax
                dec ax
                dec ax
                dec bx
                dec cx
                dec dx
            |]) $ do
                shouldEqSources [ax, bx, cx, dx]
                shouldEqSourcesAllFlags

testSub env = do
    describe "SUB [8] ACC REG <- IMM" $ do
        it "Simple sub" $ do
            runTest env ([untrimming|
                mov al, 2
                sub al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesAllFlags
        it "Add negative CF ZF" $ do
            runTest env ([untrimming|
                mov al, 1
                sub al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesAllFlags
        it "Add negative CF and OF" $ do
            runTest env ([untrimming|
                mov al, -127
                sub al, 120
            |]) $ do
                shouldEqSources al
                shouldEqSourcesAllFlags
    describe "SUB [16] ACC REG <- IMM" $ do
        it "Simple sub" $ do
            runTest env ([untrimming|
                mov ax, 2
                sub ax, 1
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesAllFlags
        it "Add negative CF ZF" $ do
            runTest env ([untrimming|
                mov ax, 1
                sub ax, 1
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesAllFlags
        it "Add negative CF and OF" $ do
            runTest env ([untrimming|
                mov ax, -32123
                sub ax, 31234
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesAllFlags
    describe "SUB [8] REG <- REG" $ do
        it "Simple sub" $ do
            runTest env ([untrimming|
                mov al, 1
                mov bl, 2
                sub al, bl
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesAllFlags
        it "Add negative CF and OF" $ do
            runTest env ([untrimming|
                mov al, -127
                mov bl, 120
                sub bl, al
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesAllFlags
    describe "SUB [16] REG <- REG" $ do
        it "Simple sub" $ do
            runTest env ([untrimming|
                mov ax, 1
                mov bx, 2
                add ax, bx
            |]) $ do
                shouldEqSources [ax, bx]
                shouldEqSourcesAllFlags
        it "Add negative CF and OF" $ do
            runTest env ([untrimming|
                mov ax, -32123
                mov bx, 31234
                sub bx, ax
            |]) $ do
                shouldEqSources [ax, bx]
                shouldEqSourcesAllFlags
    describe "SBB [16] REG <- REG" $ do
        it "Simple sbb" $ do
            runTest env ([untrimming|
                mov ax, 2
                mov bx, 1
                sbb bx, ax
                mov ax, 2
                mov bx, 1
                sbb bx, ax
            |]) $ do
                shouldEqSources [ax, bx]
                shouldEqSourcesAllFlags
    describe "CMP [16] REG, IMM" $ do
        it "Simple cmp" $ do
            runTest env ([untrimming|
                mov ax, 10
                cmp ax, 1
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesAllFlags

testArithOther env = do
    describe "CBW/CWD" $ do
        it "cbw no sign" $ do
            runTest env ([untrimming|
                mov al, 0x7F
                cbw
            |]) $ do
                shouldEqSources ax
        it "cbw sign" $ do
            runTest env ([untrimming|
                mov al, -1
                cbw
            |]) $ do
                shouldEqSources ax
        it "cwd no sign" $ do
            runTest env ([untrimming|
                mov ax, 0x7FFF
                cwd
            |]) $ do
                shouldEqSources [ax, dx]
        it "cwd sign" $ do
            runTest env ([untrimming|
                mov ax, -1
                cwd
            |]) $ do
                shouldEqSources ax
    describe "NEG" $ do
        it "simple" $ do
            runTest env ([untrimming|
                mov ax, 1
                neg ax
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesAllFlags
        it "simple negative" $ do
            runTest env ([untrimming|
                mov ax, -11
                neg ax
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesAllFlags
        it "CF" $ do
            runTest env ([untrimming|
                mov ax, 0
                neg ax
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesAllFlags
        it "OF" $ do
            runTest env ([untrimming|
                mov al, -128
                neg al
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesAllFlags

testArithAAA env = do
    describe "AAA" $ do
        it "simple" $ do
            runTest env ([untrimming|
                mov al, 99
                aaa
            |]) $ do
                shouldEqSources ax

testArithMuldiv env = do
    describe "MUL" $ do
        it "8 no OF/CF" $ do
            runTest env ([untrimming|
                mov al, 10
                mov bl, 20
                mul bl
            |]) $ do
                shouldEqSources ax
        it "8 OF/CF" $ do
            runTest env ([untrimming|
                mov al, 100
                mov bl, 20
                mul bl
            |]) $ do
                shouldEqSources ax
        it "16 no OF/CF" $ do
            runTest env ([untrimming|
                mov ax, 10
                mov bx, 200
                mul bx
            |]) $ do
                shouldEqSources [ax, dx]
        it "16 OF/CF" $ do
            runTest env ([untrimming|
                mov ax, 255
                mov bx, 300
                mul bx
            |]) $ do
                shouldEqSources [ax, dx]
    describe "IMUL" $ do
        it "8 no OF/CF" $ do
            runTest env ([untrimming|
                mov al, -10
                mov bl, 20
                imul bl
            |]) $ do
                shouldEqSources ax
        it "8 no OF/CF" $ do
            runTest env ([untrimming|
                mov al, -10
                mov bl, -20
                imul bl
            |]) $ do
                shouldEqSources ax
        it "8 OF/CF" $ do
            runTest env ([untrimming|
                mov al, 100
                mov bl, -20
                imul bl
            |]) $ do
                shouldEqSources ax
        it "8 OF/CF" $ do
            runTest env ([untrimming|
                mov al, -10
                mov bl, -20
                imul bl
            |]) $ do
                shouldEqSources ax
    describe "DIV" $ do
        it "8 (>= 1)" $ do
            runTest env ([untrimming|
                mov ax, 1000
                mov bl, 50
                div bl
            |]) $ do
                shouldEqSources ax
        it "8 (< 1)" $ do
            runTest env ([untrimming|
                mov ax, 20
                mov bl, 50
                div bl
            |]) $ do
                shouldEqSources ax
        it "8 (>= 1) neg 1" $ do
            runTest env ([untrimming|
                mov ax, -1000
                mov bl, 50
                idiv bl
            |]) $ do
                shouldEqSources ax
        it "8 (>= 1) neg 2" $ do
            runTest env ([untrimming|
                mov ax, -1000
                mov bl, -50
                idiv bl
            |]) $ do
                shouldEqSources ax
-------------------------------------------------------------------------------
