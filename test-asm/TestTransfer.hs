{-# LANGUAGE QuasiQuotes #-}

module TestTransfer where

import Test.Hspec
import Test.Hspec.Core.Runner

import Prism.Cpu
import Prism.Instructions
import Prism.Command

import TestAsm.Run
import TestAsm.Common

import NeatInterpolation

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
        it "MOV IMM16 to SP, BP, SI, DI" $ do
            execPrism [(sp `shouldEq` 0x1001), (bp `shouldEq` 0x1002), (si `shouldEq` 0x1003), (di `shouldEq` 0x1004)] env $ [text|
                mov sp, 0x1001
                mov bp, 0x1002
                mov si, 0x1003
                mov di, 0x1004
            |]
        it "MOV REG to SP, BP, SI, DI" $ do
            execPrism [(sp `shouldEq` 0x1001), (bp `shouldEq` 0x1002), (si `shouldEq` 0x1003), (di `shouldEq` 0x1004)] env $ [text|
                mov ax, 0x1001
                mov bx, 0x1002
                mov cx, 0x1003
                mov dx, 0x1004
                mov sp, ax
                mov bp, bx
                mov si, cx
                mov di, dx
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
        it "PUSH BP" $ do
            execAndCmpNF [ax, bx, cx, dx] env $ [text|
                mov bp, 0x1010
                mov bx, 0x2002
                mov cx, 0x3003
                mov dx, 0x4004
                push bp
                ;push bx
                ;push cx
                ;push dx
                pop ax
                ;pop bx
                ;pop cx
                ;pop dx
            |]
        it "Test LEA" $ do
            execPrism [(ax `shouldEq` (0x2002 + 120))] env [text|
                mov bx, 0x2002
                lea ax, [bx + 120]
            |]
        it "Test LEA disp16" $ do
            execPrism [(ax `shouldEq` (0x2002 + 0x1001))] env [text|
                mov bx, 0x2002
                lea ax, [bx + 0x1001]
            |]
        it "Test LEA neg disp" $ do
            execPrism [(ax `shouldEq` (0x2002 - 1))] env [text|
                mov bx, 0x2002
                lea ax, WORD [bx - 1]
            |]
        it "Test LEA neg disp16" $ do
            execPrism [(ax `shouldEq` (0x2002 - 0x1000))] env [text|
                mov bx, 0x2002
                lea ax, WORD [bx - 0x1000]
            |]
        it "Test LES" $ do
            execPrism [(ax `shouldEq` 0x1234), (dx `shouldEq` 0x0060)] env [text|
                absolute 0x100
                    reg_mem    resw    1
                    seg_mem    resw    1
                section .text
                org 0
                    mov ax, 0
                    mov es, ax
                    mov ds, ax
                    mov [reg_mem], WORD 0x1234
                    mov [seg_mem], WORD 0x0060
                    les ax, [reg_mem]
                    mov dx, es
            |]
        it "Test LDS" $ do
            execPrism [(ax `shouldEq` 0x1234), (dx `shouldEq` 0x0060)] env [text|
                absolute 0x100
                    reg_mem    resw    1
                    seg_mem    resw    1
                section .text
                org 0
                    mov ax, 0
                    mov es, ax
                    mov ds, ax
                    mov [reg_mem], WORD 0x1234
                    mov [seg_mem], WORD 0x0060
                    lds ax, [reg_mem]
                    mov dx, ds
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
            execPrism [(ax `shouldEq` 0xFFAA), (bx `shouldEq` 0xDDCC), (dx `shouldEq` 100)] env [text|
                mov bx, bp
                sub bx, 10
                ;add bx, 4
                mov [ss:bx], WORD 100
                mov [bp], WORD 0xFFAA
                mov ax, [bp]
                mov [bp + 10], WORD 0xDDCC
                mov bx, [bp + 10]
                mov dx, [bp - 10]
                ;mov dx, [bp + 4]
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
    describe "XLAT" $ do
        it "XLAT simple" $ do
            comm <- newPrismComm False
            execPrismHalt [(ax `shouldEq` 0x15)] env comm [text|
                SECTION .data start=100h
                    pxlat_table db 0x11, 0x12, 0x13, 0x14, 0x15
                    times 256-($-$$) db 0xAF
                section .text
                org 0
                    mov ax, cs
                    mov ds, ax
                    mov bx, pxlat_table
                    mov ax, 4
                    xlatb
                    hlt
            |]
        it "XLAT" $ do
            comm <- newPrismComm False
            execPrismHalt [(bx `shouldEq` 0x15), (dx `shouldEq` 0x11), (cx `shouldEq` 0xAF)] env comm [text|
                SECTION .data start=100h
                    pxlat_table db 0x11, 0x12, 0x13, 0x14, 0x15
                    times 256-($-$$) db 0xAF
                section .text
                org 0
                    mov ax, cs
                    mov ds, ax
                    mov bx, pxlat_table
                    mov ax, 10
                    xlatb
                    mov cx, ax
                    mov ax, 0
                    xlatb
                    mov dx, ax
                    mov ax, 4
                    xlatb
                    mov bx, ax
                    hlt
            |]

-------------------------------------------------------------------------------
