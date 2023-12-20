{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module TestFlags where

import Test.Hspec

import Prism.Cpu
import Prism.Instructions

import TestAsm.Run
import TestAsm.Common

import NeatInterpolation

-------------------------------------------------------------------------------

testFlagsZF env = 
    describe "Flags ZF" $ do
        --todo
        {-it "ZF set" $ do
            runTest env ([untrimming|
                mov ax, 0
            |]) $ do
                --shouldEq1 ZF True
                shouldEqSources ZF-}
        it "ZF cleared" $ do
            runTest env ([untrimming|
                mov al, 0
                add al, 1
            |]) $ do
                shouldEq1 ZF False
                shouldEqSources ZF

testFlagsCF env = 
    describe "Flags CF" $ do
        it "CF cleared" $ do
            runTest env ([untrimming|
                mov al, 10
                add al, 245
            |]) $ do
                shouldEq1 CF False
                shouldEq1 OF False
                --shouldEqSources [CF, OF]
        it "CF cleared sub" $ do
            runTest env ([untrimming|
                mov al, 0x80
                sub al, 1
            |]) $ do
                showAllRegsL
                showAllRegsR
                shouldEq1 al 127
                shouldEq1 CF False
                shouldEq1 OF True
                shouldEqSources CF
                shouldEqSources OF
                --shouldEqSources [CF, OF]
                shouldEqSources al
        it "CF set" $ do
            runTest env ([untrimming|
                mov al, 1
                add al, 255
            |]) $ do
                shouldEq1 CF True
                shouldEqSources CF
                shouldEqSources al
        it "CF set negative" $ do
            runTest env ([untrimming|
                mov al, 127
                add al, -120
            |]) $ do
                shouldEq1 CF True
                shouldEq1 OF False
                shouldEqSources [CF, OF]
        it "CF set sub" $ do
            runTest env ([untrimming|
                mov al, 1
                sub al, 2
            |]) $ do
                shouldEq1 CF True
                shouldEq1 OF False
                shouldEq1 al 255
                shouldEqSources [CF, OF]
                shouldEqSources al
        it "CF set mul" $ do
            runTest env ([untrimming|
                mov bl, 100
                mov al, 10
                mul bl
            |]) $ do
                shouldEq1 CF True
                shouldEq1 OF True
                shouldEq1 ax 1000
                shouldEqSources [CF, OF]
                shouldEqSources ax

testFlagsOF env = 
    describe "Flags OF" $ do
        it "OF cleared" $ do
            runTest env ([untrimming|
                mov al, -127
                add al, 255
            |]) $ do
                shouldEq1 al (-128)
                shouldEq1 OF False
                shouldEq1 CF True
        it "OF set when ADD" $ do
            runTest env ([untrimming|
                mov al, -127
                add al, 127
            |]) $ do
                shouldEq1 OF False
                shouldEq1 CF True
        it "OF set when ADD -2" $ do
            runTest env ([untrimming|
                mov al, -2
                add al, -2
            |]) $ do
                shouldEq1 OF False
                shouldEq1 CF True
        it "OF set when ADD negative" $ do
            runTest env ([untrimming|
                mov al, -127
                add al, -120
            |]) $ do
                shouldEq1 OF True
                shouldEq1 CF True
                shouldEq1 al 9
        it "OF set when SUB" $ do
            runTest env ([untrimming|
                mov al, -127
                sub al, 120
            |]) $ do
                shouldEq1 OF True
                shouldEq1 CF False
                shouldEq1 al 9
        it "OF set when SUB negative" $ do
            runTest env ([untrimming|
                mov al, -127
                sub al, -120
            |]) $ do
                shouldEq1 OF False
                shouldEq1 CF True
