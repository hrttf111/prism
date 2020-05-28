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
