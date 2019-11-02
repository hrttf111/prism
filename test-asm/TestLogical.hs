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
