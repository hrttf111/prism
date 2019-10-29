{-# LANGUAGE QuasiQuotes #-}

import Test.Hspec

import Assembler
import TestCommon
import TestFlags
import TestTransfer
import TestArithmetic

import PrismCpu
import Instruction.Transfer

import NeatInterpolation
import Data.Text (Text)

import Control.Monad.Trans (MonadIO, liftIO)

instrList = transferInstrList

main :: IO ()
main = do
    execC <- createTestEnv 
    env <- createTestEnv2 instrList
    hspec $ do
        testMov env
        testAdd env
        testFlagsOF env
        testFlagsCF env
        testFlagsZF env
