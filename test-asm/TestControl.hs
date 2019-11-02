{-# LANGUAGE QuasiQuotes #-}

module TestControl where

import Test.Hspec

import Prism
import PrismCpu
import Instruction.Transfer

import TestCommon

import NeatInterpolation

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
