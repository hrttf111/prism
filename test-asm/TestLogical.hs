{-# LANGUAGE QuasiQuotes #-}

module TestLogical where

import Test.Hspec

import Prism
import PrismCpu
import Instruction.Transfer

import TestCommon

import NeatInterpolation

testLog env = do
    describe "NOT" $ do
        it "8" $ do
            execAndCmpNF [al, bl, dl, cl] env $ [text|
                mov al, 1
                mov bl, -1
                mov cl, 0xFF
                mov dl, 0
                not al
                not bl
                not cl
                not dl
            |]
        it "16" $ do
            execAndCmpNF [ax, bx, dx, cx] env $ [text|
                mov ax, 1
                mov bx, -1
                mov cx, 0xFFFF
                mov dx, 0
                not ax
                not bx
                not cx
                not dx
            |]
    describe "TEST" $ do
        it "8" $ do
            execAndCmp [al] env $ [text|
                mov al, 1
                test al, 1
            |]
        it "16" $ do
            execAndCmp [ax] env $ [text|
                mov ax, 1
                test ax, 1
            |]
    describe "SHL" $ do
        it "8" $ do
            execAndCmp [al] env $ [text|
                mov al, 1
                shl al, 1
            |]
        it "8 neg" $ do
            execAndCmp [al, bl] env $ [text|
                mov al, 0xAF
                mov cl, 1
                shl al, cl
                mov bl, 0x80
                shl bl, 1
            |]
        it "8 multi" $ do
            execAndCmp [al, bl] env $ [text|
                mov al, 0
                shl al, 1
                mov bl, 0x0F
                mov cl, 4
                shl bl, cl
            |]
        it "8 multi2" $ do
            --todo: OF is undefined need to cmp without OF in this case
            execAndCmpNF [al, dl] env $ [text|
                mov al, 0
                shl al, 1
                mov dl, 0xAF
                mov cl, 2
                shl dl, cl
            |]
    describe "SHR" $ do
        it "8" $ do
            execAndCmp [al] env $ [text|
                mov al, 1
                shr al, 1
            |]
        it "8 neg" $ do
            --todo: OF is undefined need to cmp without OF in this case
            execAndCmpNF [al] env $ [text|
                mov al, 0
                shl al, 1
                mov al, 0xAF
                mov cl, 2
                shr al, cl
            |]
    describe "ROL" $ do
        it "8" $ do
            execAndCmpNF [al] env $ [text|
                mov al, 0x81
                rol al, 1
            |]
        it "8 multi" $ do
            execAndCmpNF [al, bl] env $ [text|
                mov al, 0x7F
                mov cl, 2
                rol al, cl
                mov bl, 0x7F
                mov cl, 4
                rol al, cl
            |]
    describe "ROR" $ do
        it "8" $ do
            execAndCmpNF [al] env $ [text|
                mov al, 0x81
                ror al, 1
            |]
        it "8 multi" $ do
            execAndCmpNF [al, bl] env $ [text|
                mov al, 0x7F
                mov cl, 2
                ror al, cl
                mov bl, 0x7F
                mov cl, 4
                ror al, cl
            |]
    describe "RCL" $ do
        it "8 CF=0" $ do
            execAndCmpNF [al] env $ [text|
                clc
                mov al, 0x81
                rcl al, 1
            |]
        it "8 CF=1" $ do
            execAndCmpNF [al] env $ [text|
                stc
                mov al, 0x81
                rcl al, 1
            |]
        it "8 multi" $ do
            execAndCmpNF [al, bl] env $ [text|
                clc
                mov al, 0x7F
                mov cl, 2
                rcl al, cl
                mov bl, 0x7F
                mov cl, 4
                rcl al, cl
            |]
    describe "RCR" $ do
        it "8 CF=0" $ do
            execAndCmpNF [al] env $ [text|
                clc
                mov al, 0x81
                rcr al, 1
            |]
        it "8 CF=1" $ do
            execAndCmpNF [al] env $ [text|
                stc
                mov al, 0x81
                rcr al, 1
            |]
        it "8 multi" $ do
            execAndCmpNF [al, bl] env $ [text|
                clc
                mov al, 0xF7
                mov cl, 2
                rcr al, cl
                mov bl, 0xF7
                mov cl, 4
                rcr al, cl
            |]
