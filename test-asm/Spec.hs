{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

import Test.Hspec
import Test.Hspec.Core.Runner

import Control.Monad.Trans (MonadIO, liftIO)
import Data.Text (Text)

import NeatInterpolation

import Infra

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
            makerP = PrismEnvMaker
        testMovMem2 makerPQ
        testMov makerPQ
        testMovLDS makerP
        testMovMem1 makerPQ
        testMovXlat makerP
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
        testProcessor makerP
        testPeripheral
        testPC

main :: IO ()
main = do
    runSpec doTests defaultConfig {configConcurrentJobs=(Just 1)}
    return ()

-------------------------------------------------------------------------------
