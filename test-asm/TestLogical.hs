{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module TestLogical where

import Test.Hspec

import NeatInterpolation

import Prism.Cpu
import Infra

-------------------------------------------------------------------------------

shouldEqSourcesIgnoreOF :: (ShouldEqSources [Flag] m) => m ()
shouldEqSourcesIgnoreOF = shouldEqSources [CF, PF, AF, ZF, SF]

shouldEqSourcesIgnoreAF :: (ShouldEqSources [Flag] m) => m ()
shouldEqSourcesIgnoreAF = shouldEqSources [CF, PF, ZF, SF, OF]

shouldEqSourcesIgnoreOFAndAF :: (ShouldEqSources [Flag] m) => m ()
shouldEqSourcesIgnoreOFAndAF = shouldEqSources [CF, PF, ZF, SF]

shouldEqSourcesIgnoreNonRotate :: (ShouldEqSources [Flag] m) => m ()
shouldEqSourcesIgnoreNonRotate = shouldEqSources [CF, OF]

testLog env = do
    describe "NOT" $ do
        it "8" $ do
            runTest env ([untrimming|
                mov al, 1
                mov bl, -1
                mov cl, 0xFF
                mov dl, 0
                not al
                not bl
                not cl
                not dl
            |]) $ do
                shouldEqSources [al, bl, cl, dl]
        it "16" $ do
            runTest env ([untrimming|
                mov ax, 1
                mov bx, -1
                mov cx, 0xFFFF
                mov dx, 0
                not ax
                not bx
                not cx
                not dx
            |]) $ do
                shouldEqSources [ax, bx, cx, dx]
    describe "TEST" $ do
        it "8" $ do
            runTest env ([untrimming|
                mov al, 1
                test al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesAllFlags
        it "16" $ do
            runTest env ([untrimming|
                mov ax, 1
                test ax, 1
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesAllFlags
        it "8_2" $ do
            runTest env ([untrimming|
                mov al, 0x10
                test al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesAllFlags
        it "16_2" $ do
            runTest env ([untrimming|
                mov ax, 0x0100
                test ax, 1
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesAllFlags
    describe "SHL" $ do
        it "8" $ do
            runTest env ([untrimming|
                mov al, 1
                shl al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesIgnoreAF
        it "8 neg" $ do
            runTest env ([untrimming|
                mov al, 0xAF
                mov cl, 1
                shl al, cl
                mov bl, 0x80
                shl bl, 1
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesIgnoreAF
        it "8 multi" $ do
            runTest env ([untrimming|
                mov al, 0
                shl al, 1
                mov bl, 0x0F
                mov cl, 4
                shl bl, cl
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesIgnoreOFAndAF
        it "8 multi2" $ do
            runTest env ([untrimming|
                mov al, 0
                shl al, 1
                mov dl, 0xAF
                mov cl, 2
                shl dl, cl
            |]) $ do
                shouldEqSources [al, dl]
                shouldEqSourcesIgnoreOFAndAF
    describe "SHR" $ do
        it "8" $ do
            runTest env ([untrimming|
                mov al, 1
                shr al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesIgnoreAF
        it "8 neg" $ do
            runTest env ([untrimming|
                mov al, 0
                shl al, 1
                mov al, 0xAF
                mov cl, 2
                shr al, cl
            |]) $ do
                shouldEqSources al
                shouldEqSourcesIgnoreAF
    describe "SAR" $ do
        it "8" $ do
            runTest env ([untrimming|
                mov al, 1
                sar al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesIgnoreAF
        it "8 neg" $ do
            runTest env ([untrimming|
                mov al, 0xAF
                mov cl, 2
                sar al, cl
            |]) $ do
                shouldEqSources al
                shouldEqSourcesIgnoreAF
        it "16" $ do
            runTest env ([untrimming|
                mov ax, 0x101
                sar ax, 1
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesIgnoreAF
        it "16 neg" $ do
            runTest env ([untrimming|
                mov ax, 0xAFFF
                mov cl, 12
                sar ax, cl
            |]) $ do
                shouldEqSources ax
                shouldEqSourcesIgnoreAF
    describe "ROL" $ do
        it "8" $ do
            runTest env ([untrimming|
                mov al, 0x81
                rol al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesIgnoreNonRotate
        it "8 multi" $ do
            runTest env ([untrimming|
                mov al, 0x7F
                mov cl, 2
                rol al, cl
                mov bl, 0x7F
                mov cl, 4
                rol al, cl
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesIgnoreNonRotate
    describe "ROR" $ do
        it "8" $ do
            runTest env ([untrimming|
                mov al, 0x81
                ror al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesIgnoreNonRotate
        it "8 multi" $ do
            runTest env ([untrimming|
                mov al, 0x7F
                mov cl, 2
                ror al, cl
                mov bl, 0x7F
                mov cl, 4
                ror al, cl
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSourcesIgnoreNonRotate
    describe "RCL" $ do
        it "8 CF=0" $ do
            runTest env ([untrimming|
                clc
                mov al, 0x81
                rcl al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesIgnoreNonRotate
        it "8 CF=1" $ do
            runTest env ([untrimming|
                stc
                mov al, 0x81
                rcl al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesIgnoreNonRotate
        it "8 multi" $ do
            runTest env ([untrimming|
                clc
                mov al, 0x7F
                mov cl, 2
                rcl al, cl
                mov bl, 0x7F
                mov cl, 4
                rcl al, cl
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSources CF
    describe "RCR" $ do
        it "8 CF=0" $ do
            runTest env ([untrimming|
                clc
                mov al, 0x81
                rcr al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesIgnoreNonRotate
        it "8 CF=1" $ do
            runTest env ([untrimming|
                stc
                mov al, 0x81
                rcr al, 1
            |]) $ do
                shouldEqSources al
                shouldEqSourcesIgnoreNonRotate
        it "8 multi" $ do
            runTest env ([untrimming|
                clc
                mov al, 0xF7
                mov cl, 2
                rcr al, cl
                mov bl, 0xF7
                mov cl, 4
                rcr al, cl
            |]) $ do
                shouldEqSources [al, bl]
                shouldEqSources CF

-------------------------------------------------------------------------------
