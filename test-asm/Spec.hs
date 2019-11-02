{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec

import Assembler
import TestCommon
import TestFlags
import TestTransfer
import TestArithmetic

import PrismCpu
import Instruction.Transfer
import Instruction.Arithmetic
import Instruction.Processor

import NeatInterpolation
import Data.Text (Text)

import Control.Monad.Trans (MonadIO, liftIO)

instrList = transferInstrList ++ arithmeticInstrList ++ processorInstrList

main :: IO ()
main = do
    execC <- createTestEnv 
    env <- createTestEnv2 instrList
    hspec $ do
        testMov env
        testAdd env
        testSub env
        testInc env
        testArithOther env
        --testFlagsOF env
        --testFlagsCF env
        --testFlagsZF env
