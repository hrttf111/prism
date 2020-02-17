{-# LANGUAGE QuasiQuotes #-}

module TestPC where

import Test.Hspec

import Data.IORef
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Concurrent

import Prism
import PrismCpu
import PrismPeripheral
import PrismInterrupt
import PrismCommand

import Peripherals.Local
import Peripherals.PC

import TestCommon

import NeatInterpolation

-------------------------------------------------------------------------------

instance PeripheralsTestCreator PC where
    createTestPeripherals (PeripheralLocal maxPorts maxMem portRegion memRegion ports mem devices) queue = do
        ref <- newIORef devices
        return $ IOCtx (PeripheralsLocal maxPorts maxMem ports mem queue ref) memRegion portRegion

type TestInterruptHandler = Uint8 -> Uint8 -> Uint8

testInterruptHandler :: TestInterruptHandler -> InterruptHandler
testInterruptHandler handler ctx int =
    (liftIO $ putStrLn "In handler") >> (handler int <$> readReg8 memReg al) >>= writeReg8 memReg al >> return ctx
    where
        memReg = ctxReg ctx

-------------------------------------------------------------------------------

testPC instrList = do
    describe "Test PC PIC" $ do
        let devR = 0
        it "Init PIC" $ do
            let devices = createPC
                testHandler int val = 89
                intList = [
                    (PrismInt 0x10, testInterruptHandler testHandler)
                    ]
            comm <- newPrismComm False
            env <- createPeripheralsTestEnv instrList devR [] [] devices pcPorts [] intList
            execPrismHalt [(al `shouldEq` 89)] env comm $ [text|
                mov al, 134
                int 0x10
                hlt
            |]

-------------------------------------------------------------------------------
