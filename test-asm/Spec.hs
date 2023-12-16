{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec
import Test.Hspec.Core.Runner

import Control.Monad.Trans (MonadIO, liftIO)

import NeatInterpolation
import Data.Text (Text)

import Prism.Instructions

import Assembler
import qualified Qemu

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
import qualified Data.ByteString as B
import Data.List (intercalate)
import Foreign.Ptr (castPtr)
import Prism.Cpu

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

main :: IO ()
main = do
    env <- createTestEnv x86InstrList
    runSpec (doTests env) defaultConfig {configConcurrentJobs=(Just 1)}
    res <- Qemu.execCode $ [untrimming|
        mov WORD [0x9800], 0x1234
        mov ax, 1
        mov bx, 2
        mov cx, 3
        mov dx, WORD [0x9800]
    |]
    printRes res
    return ()
    where
        printRes (Right (Qemu.AsmRes regs _ _)) = do
            B.useAsCStringLen regs (\(ptr, len) ->
                putStrLn =<< ((intercalate "\n") <$> (printRegs $ MemReg $ castPtr ptr))
                )
        printRes (Left err) =
            putStrLn err

-------------------------------------------------------------------------------
