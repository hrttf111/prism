{-# LANGUAGE QuasiQuotes #-}

module TestPC where

import Test.Hspec

import Data.IORef
import Control.Concurrent

import Prism
import PrismCpu
import PrismPeripheral
import PrismInterrupt

import Peripherals.Local
import Peripherals.PC

import TestCommon

import NeatInterpolation

-------------------------------------------------------------------------------

instance PeripheralsTestCreator PC where
    createTestPeripherals (PeripheralLocal maxPorts maxMem portRegion memRegion ports mem devices) queue = do
        ref <- newIORef devices
        return $ IOCtx (PeripheralsLocal maxPorts maxMem ports mem queue ref) memRegion portRegion

-------------------------------------------------------------------------------

testPC instrList = do
    describe "Test PC PIC" $ do
        let devR = 0
        it "Init PIC" $ do
            let devices = createPC
                val = 134
            env <- createPeripheralsTestEnv instrList devR [] [] devices pcPorts []
            execPrism [(al `shouldEq` 134)] env $ [text|
                mov al, 134
            |]

-------------------------------------------------------------------------------
