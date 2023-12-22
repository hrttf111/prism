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

-------------------------------------------------------------------------------

doTests = do
        let makerPQ = PrismQemuEnvMaker
            makerPN = PrismNativeEnvMaker
        testMovMem2 makerPQ
        testMov makerPQ
        testMovLDS PrismEnvMaker
        testMovMem1 makerPQ
        testMovXlat PrismEnvMaker
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
        testString makerPQ
        testProcessor PrismEnvMaker
        testPeripheral
        testPC

main :: IO ()
main = do
    runSpec doTests defaultConfig {configConcurrentJobs=(Just 1)}
    return ()

-------------------------------------------------------------------------------
