{-# LANGUAGE QuasiQuotes #-}

module TestTransfer where

import Test.Hspec

import Prism
import PrismCpu
import Instruction.Transfer

import TestCommon

import NeatInterpolation

testMov env = 
    describe "MOV [8] REG <- IMM" $ do
        it "All regs" $ do
            execAndCmpNF [al, bl, cl, dl, ah, bh, ch, dh] env $ [text|
                mov al, 11
                mov bl, 12
                mov cl, 13
                mov dl, 14
                mov ah, 15
                mov bh, 16
                mov ch, 17
                mov dh, 18
            |]
        it "All regs1" $ do
            execAndCmpNF [al, bl, cl, dl, ah, bh, ch, dh] env $ [text|
                mov al, 11
                mov bl, al
                mov cl, bl
                mov dl, dl
                mov ah, dl
                mov bh, ah
                mov ch, bh
                mov dh, ch
                mov al, dh
            |]
        it "MOV IMM16 to Reg16" $ do
            execAndCmpNF [ax, bx, cx, dx] env $ [text|
                mov ax, 0x1001
                mov bx, 0x2002
                mov cx, 0x3003
                mov dx, 0x4004
            |]
        it "XCHG to accumulator Reg8" $ do
            execAndCmpNF [al, bl, cl, dl] env $ [text|
                mov al, 0x10
                mov bl, 0x20
                mov cl, 0x30
                mov dl, 0x40
                xchg al, bl
                xchg cl, dl
            |]
        it "XCHG to accumulator Reg16" $ do
            execAndCmpNF [ax, bx, cx, dx] env $ [text|
                mov ax, 0x1001
                mov bx, 0x2002
                mov cx, 0x3003
                mov dx, 0x4004
                xchg ax, bx
                xchg cx, dx
            |]
        it "PUSH Reg16" $ do
            execAndCmpNF [ax, bx, cx, dx] env $ [text|
                mov ax, 0x1001
                mov bx, 0x2002
                mov cx, 0x3003
                mov dx, 0x4004
                push ax
                push bx
                push cx
                push dx
                pop ax
                pop bx
                pop cx
                pop dx
            |]
        it "Test LEA" $ do
            code <- (assembleNative16 env) $ [text|
                mov bx, 0x2002
                lea ax, [bx + 120]
            |]
            ctx <- (executePrism env) code
            let memRegP = ctxReg ctx
            ax `shouldEq` (0x2002 + 120) $ memRegP
