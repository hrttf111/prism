{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module TestProcessor where

import Test.Hspec

import NeatInterpolation

import Prism.Cpu
import Infra

-------------------------------------------------------------------------------

testProcessor env = do
    describe "Software interrupts" $ do
        it "Int 5" $ do
            runTest env ([untrimming|
                xor ax, ax
                xor cx, cx
                mov ds, cx
                mov bx, cs
                mov [20], WORD INTERRUPT1
                mov [22], bx
                mov [24], WORD INTERRUPT2
                mov [26], bx
                int 5
                jmp END1

                INTERRUPT1: 
                mov ax, 0xFFAA
                int 6
                iret

                INTERRUPT2:
                mov cx, 0xDDFF
                iret

                END1:
                mov bx, 0xFFAA
            |]) $ do
                shouldEq ax 0xFFAA
                shouldEq bx 0xFFAA
                shouldEq cx 0xDDFF
        {-it "Single Step" $ do
            execPrism [(ax `shouldEq` 0xFFAA), (bx `shouldEq` 0xFFAA), (cx `shouldEq` 5)] env $ [text|
                xor ax, ax
                xor cx, cx
                mov ds, cx
                mov bx, cs
                mov [4], WORD INTERRUPT1
                mov [6], bx
                jmp END1

                INTERRUPT1: 
                inc cx
                iret

                END1:
                mov bx, 0xFFAA
            |]
            -}

-------------------------------------------------------------------------------
