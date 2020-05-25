{-# LANGUAGE QuasiQuotes #-}

module TestTransferNew where

import Test.Hspec
import Test.Hspec.Core.Runner

import Prism.Cpu
import Prism.Instructions

import TestAsm.Run
import TestAsm.Common

import TestArithmeticNew
import TestLogicalNew
import TestControlNew
import TestStringNew
import TestFlagsNew
import TestProcessorNew
import TestPeripheralsNew

import NeatInterpolation

-------------------------------------------------------------------------------

instrList = x86InstrList

doTests env = do
        testMov env
        testMovMem env
        testAdd env
        testInc env
        testSub env
        testArithOther env
        testArithMuldiv env
        testLog env
        testControl env
        testString env
        testFlagsZF env
        testFlagsCF env
        testFlagsOF env
        testProcessor env
        testPeripheral x86InstrList

testAll = do
    env <- createTestEnv instrList
    runSpec (doTests env) defaultConfig {configConcurrentJobs=(Just 1)}

-------------------------------------------------------------------------------

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
            execPrism [(ax `shouldEq` (0x2002 + 120))] env [text|
                mov bx, 0x2002
                lea ax, [bx + 120]
            |]

testMovMem env = do
    describe "MOV mem ds" $ do
        it "Mem16 direct <- Imm16" $ do
            execPrism [(ax `shouldEq` 0xFFAA), (bx `shouldEq` 0xDDCC)] env [text|
                mov [0x1000], WORD 0xFFAA
                mov ax, [0x1000]
                mov [0x1010], WORD 0xDDCC
                mov bx, [0x1010]
            |]
        it "Mem16 bx/si/di <- Imm16" $ do
            execPrism [(ax `shouldEq` 0xFFAA), (cx `shouldEq` 0xDDCC), (dx `shouldEq` 0x8877)] env [text|
                mov bx, 0x1000
                mov [bx], WORD 0xFFAA
                mov ax, [bx]
                mov si, 0x1010
                mov [si], WORD 0xDDCC
                mov cx, [si]
                mov di, 0x1020
                mov [di], WORD 0x8877
                mov dx, [di]
            |]
        it "Mem16 bx/si/di + disp16 <- Imm16" $ do
            execPrism [(ax `shouldEq` 0xFFAA), (cx `shouldEq` 0xDDCC), (dx `shouldEq` 0x8877)] env [text|
                mov bx, 0
                mov [bx + 0x1000], WORD 0xFFAA
                mov ax, [bx + 0x1000]
                mov si, 10
                mov [si + 0x1000], WORD 0xDDCC
                mov cx, [si + 0x1000]
                mov di, 20
                mov [di + 0x1000], WORD 0x8877
                mov dx, [di + 0x1000]
            |]
        it "Mem16 bx+si/bx+di + disp16<- Imm16" $ do
            execPrism [(ax `shouldEq` 0xFFAA), (cx `shouldEq` 0xDDCC)] env [text|
                mov bx, 10
                mov si, 10
                mov [bx + si + 0x1000], WORD 0xFFAA
                mov ax, [bx + si + 0x1000]
                mov di, 20
                mov [bx + di + 0x1000], WORD 0xDDCC
                mov cx, [bx + di + 0x1000]
            |]
    describe "MOV mem ds" $ do
        it "Mem16 bp <- Imm16" $ do
            execPrism [(ax `shouldEq` 0xFFAA), (bx `shouldEq` 0xDDCC)] env [text|
                mov [bp], WORD 0xFFAA
                mov ax, [bp]
                mov [bp + 10], WORD 0xDDCC
                mov bx, [bp + 10]
            |]
        it "Mem16 bp + si/di <- Imm16" $ do
            execPrism [(cx `shouldEq` 0xDDCC), (dx `shouldEq` 0x8877)] env [text|
                mov bp, 10
                mov si, 0x1010
                mov [bp + si], WORD 0xDDCC
                mov cx, [bp + si]
                mov di, 0x1020
                mov bp, 20
                mov [bp + di], WORD 0x8877
                mov dx, [bp + di]
            |]
        it "Mem16 bp + si/di + disp8/16 <- Imm16" $ do
            execPrism [(cx `shouldEq` 0xDDCC), (dx `shouldEq` 0x8877)] env [text|
                mov bp, 10
                mov si, 0x1010
                mov [bp + si + 0x10], WORD 0xDDCC
                mov cx, [bp + si + 0x10]
                mov di, 0x1020
                mov bp, 20
                mov [bp + di + 0x1000], WORD 0x8877
                mov dx, [bp + di + 0x1000]
            |]
    describe "MOV segment replacement" $ do
        it "Replace es" $ do
            execPrism [(ax `shouldEq` 0xFFAA), (bx `shouldEq` 0xFFAA)] env [text|
                mov ax, ds
                add ax, 1
                mov es, ax
                mov [es:0], WORD 0xFFAA
                mov ax, [16]
                mov bx, [ds:16]
            |]

-------------------------------------------------------------------------------
