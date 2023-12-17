{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec
import Test.Hspec.Core.Runner

import Control.Monad.Trans (MonadIO, liftIO)

import NeatInterpolation
import Data.Text (Text)

import Prism.Instructions

import Assembler
import qualified Qemu
import qualified ExecPrism as Ep

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
import Prism.Run

-------------------------------------------------------------------------------

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
        testPC x86InstrList
        describe "New tests" $ do
            it "Test1" $ do
                env <- makeEnv1
                execPrism1 env ([untrimming|
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
                    --cmpOperandVal dx 4
                    --cmpOperandVal (MemPhy8 0x8012) 13
                putStrLn "End newTests"
            it "Qemu" $ do
                let env = TestEnv1 Qemu.assembleQemu Qemu.ExecutorQemu
                execPrism1 env ([untrimming|
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
                    --showOperandVal (MemPhy8 (0x1000 + 8000))
                    showOperandVal (MemPhy16 0x9800)
                    showOperandVal (MemRange 0x7E00 0x7E20)
                putStrLn "End newTests"
            it "Both" $ do
                let runner = decodeMemIp
                prismExec <- Ep.createPrismExecutorNoIO x86InstrList runner
                let env = TestEnv2 makeAsmStr16 prismExec Qemu.assembleQemu Qemu.ExecutorQemu
                execPrism2 env ([untrimming|
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
    env <- createTestEnv x86InstrList
    runSpec (doTests env) defaultConfig {configConcurrentJobs=(Just 1)}
    return ()

-------------------------------------------------------------------------------
