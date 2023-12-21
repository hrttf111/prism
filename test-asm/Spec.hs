{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

import Test.Hspec
import Test.Hspec.Core.Runner

import Control.Monad.Trans (MonadIO, liftIO)

import NeatInterpolation
import Data.Text (Text)

import Prism.Instructions

import TestAsm.Run
import TestAsm.Common

import TestFlags
import TestTransfer
import TestArithmetic
import TestLogical
import TestControl
import TestProcessor
import TestString
import TestPeripherals
import TestPC

--For QEMU
import Prism.Cpu

-------------------------------------------------------------------------------

doTests = do
        let makerPQ = PrismQemuEnvMaker
            makerPN = PrismNativeEnvMaker
        testMovMem2 PrismEnvMaker
        testMov makerPQ
        testMovLDS PrismEnvMaker
        testMovMem1 PrismEnvMaker
        testMovXlat PrismEnvHaltMaker
        testAdd makerPN
        testInc makerPN
        testSub makerPN
        testArithOther makerPN
        testArithMuldiv makerPN
        testArithAAA makerPQ
        testControl makerPQ
        testFlagsZF makerPN
        testFlagsCF makerPN
        testFlagsOF makerPN
        testLog makerPN
        testString PrismEnvMaker
        testProcessor PrismEnvMaker
        testPeripheral
        testPC
        --
        describe "New tests" $ do
            it "Test1" $ do
                env <- makeTestEnv PrismEnvMaker
                execTestEnv env ([untrimming|
                    mov ax, 1
                    mov bx, 2
                    mov cx, 3
                    mov dx, 4
                    mov [0x1000], WORD 0xFFAA
                    mov ax, [0x1000]
                    mov [0x1010], WORD 0xDDCC
                    mov bx, [0x1010]
                |]) $ do
                    showOperandVal ax
                    showOperandVal bx
                    showOperandVal cx
                    showOperandVal dx
                    showOperandVal ds
                    showOperandVal (MemPhy8 (0x1000 + 8000))
                    showOperandVal (MemPhy16 (0x1000 + 8000))
                    showOperandVal (MemRange (0x1000 + 8000) (0x1000 + 8020))
                putStrLn "End newTests"
            it "Qemu" $ do
                env <- makeTestEnv QemuEnvMaker
                execTestEnv env ([untrimming|
                    mov WORD [0x9800], 0x1234
                    mov ax, 1
                    mov bx, 2
                    mov cx, 3
                    mov dx, WORD [0x9800]
                |]) $ do
                    showOperandVal ax
                    showOperandVal bx
                    showOperandVal cx
                    showOperandVal dx
                    showOperandVal ds
                    showOperandVal ss
                    showOperandVal sp
                    showOperandVal (MemPhy16 0x9800)
                    showOperandVal (MemRange 0x7E00 0x7E20)
                putStrLn "End newTests"
            it "Both" $ do
                env <- makeTestEnv PrismQemuEnvMaker
                execTestEnv env ([untrimming|
                    mov WORD [0x9800], 0x1234
                    mov ax, 1
                    mov bx, 2
                    mov cx, 3
                    mov dx, WORD [0x9800]
                |]) $ do
                    showAllRegs
                    showAllRegsR
                    cmpOperandVal ax 1
                    cmpOperandSources ax
                    cmpOperandsSources [ax, bx, cx, dx]
                putStrLn "End newTests"

main :: IO ()
main = do
    runSpec doTests defaultConfig {configConcurrentJobs=(Just 1)}
    return ()

-------------------------------------------------------------------------------
