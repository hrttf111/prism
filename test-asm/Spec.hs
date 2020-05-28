{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec
import Test.Hspec.Core.Runner

import Control.Monad.Trans (MonadIO, liftIO)

import NeatInterpolation
import Data.Text (Text)

import Prism.Instructions

import Assembler

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
    return ()

-------------------------------------------------------------------------------
