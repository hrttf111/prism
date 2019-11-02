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
