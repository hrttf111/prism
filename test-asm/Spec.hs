{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec

import Assembler
import TestCommon
import TestFlags
import TestTransfer
import TestArithmetic
import TestLogical
import TestControl

import PrismCpu
import Instruction.Transfer
import Instruction.Arithmetic
import Instruction.Processor
import Instruction.Logical
import Instruction.Control

import NeatInterpolation
import Data.Text (Text)

import Control.Monad.Trans (MonadIO, liftIO)

instrList = transferInstrList 
    ++ arithmeticInstrList
    ++ processorInstrList
    ++ logicalInstrList
    ++ controlInstrList

main :: IO ()
main = do
    execC <- createTestEnv 
    env <- createTestEnv2 instrList
    hspec $ do
        testMov env
        testMovMem env
        testAdd env
        testSub env
        testInc env
        testArithMuldiv env
        testArithOther env
        testLog env
        testControl env
