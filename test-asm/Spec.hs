{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec
import Test.Hspec.Core.Runner

import Assembler
import TestCommon
import TestFlags
import TestTransfer
import TestArithmetic
import TestLogical
import TestControl
import TestProcessor
import TestString

import PrismCpu
import Instruction.Transfer
import Instruction.Arithmetic
import Instruction.Processor
import Instruction.Logical
import Instruction.Control
import Instruction.String

import NeatInterpolation
import Data.Text (Text)

import Control.Monad.Trans (MonadIO, liftIO)

instrList = transferInstrList 
    ++ arithmeticInstrList
    ++ processorInstrList
    ++ logicalInstrList
    ++ controlInstrList
    ++ stringInstrList
    ++ repInstrList stringInstrList

doTests env = do
        testMov env
        testMovMem env
        testAdd env
        testSub env
        testInc env
        testArithMuldiv env
        testArithOther env
        testLog env
        testControl env
        testProcessor env
        testString env

main :: IO ()
main = do
    execC <- createTestEnv 
    env <- createTestEnv2 instrList
    runSpec (doTests env) defaultConfig {configConcurrentJobs=(Just 1)}
    return ()
