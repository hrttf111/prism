{-# LANGUAGE QuasiQuotes #-}

module TestTransfer where

import Test.Hspec

import Prism
import PrismCpu
import Instruction.Transfer

import TestCommon

import NeatInterpolation

testMov env = 
    describe "Test mov reg8 imm8" $ do
        it "MOV IMM8 to Reg8" $ do
            code <- (assembleNative env) $ [text|
                mov al, 11
                mov bl, 12
                mov cl, 13
                mov dl, 14
                mov ah, 15
                mov bh, 16
                mov ch, 17
                mov dh, 18
            |]
            memRegN <- (executeNative env) code
            ctx <- (executePrism env) code
            let memRegP = ctxReg ctx
            al `shouldEqReg` memRegN $ memRegP
            bl `shouldEqReg` memRegN $ memRegP
            cl `shouldEqReg` memRegN $ memRegP
            dl `shouldEqReg` memRegN $ memRegP
            ah `shouldEqReg` memRegN $ memRegP
            bh `shouldEqReg` memRegN $ memRegP
            ch `shouldEqReg` memRegN $ memRegP
            dh `shouldEqReg` memRegN $ memRegP
        it "MOV IMM16 to Reg16" $ do
            code <- (assembleNative env) $ [text|
                mov ax, 0x1001
                mov bx, 0x2002
                mov cx, 0x3003
                mov dx, 0x4004
            |]
            memRegN <- (executeNative env) code
            ctx <- (executePrism env) code
            let memRegP = ctxReg ctx
            ax `shouldEqReg` memRegN $ memRegP
            bx `shouldEqReg` memRegN $ memRegP
            cx `shouldEqReg` memRegN $ memRegP
            dx `shouldEqReg` memRegN $ memRegP
        it "XCHG to accumulator Reg8" $ do
            code <- (assembleNative env) $ [text|
                mov al, 0x10
                mov bl, 0x20
                mov cl, 0x30
                mov dl, 0x40
                xchg al, bl
                xchg cl, dl
            |]
            memRegN <- (executeNative env) code
            ctx <- (executePrism env) code
            let memRegP = ctxReg ctx
            al `shouldEqReg` memRegN $ memRegP
            bl `shouldEqReg` memRegN $ memRegP
            cl `shouldEqReg` memRegN $ memRegP
            dl `shouldEqReg` memRegN $ memRegP
        it "XCHG to accumulator Reg16" $ do
            code <- (assembleNative env) $ [text|
                mov ax, 0x1001
                mov bx, 0x2002
                mov cx, 0x3003
                mov dx, 0x4004
                xchg ax, bx
                xchg cx, dx
            |]
            memRegN <- (executeNative env) code
            ctx <- (executePrism env) code
            let memRegP = ctxReg ctx
            ax `shouldEqReg` memRegN $ memRegP
            bx `shouldEqReg` memRegN $ memRegP
            cx `shouldEqReg` memRegN $ memRegP
            dx `shouldEqReg` memRegN $ memRegP
