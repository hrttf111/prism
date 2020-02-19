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

testSendIRQUp :: PrismCmdQueue -> PrismIRQ -> InterruptHandler
testSendIRQUp queue irq ctx _ =
    sendCpuMsgIO queue (PCmdInterruptUp irq) >> return ctx

testSendIRQDown :: PrismCmdQueue -> PrismIRQ -> InterruptHandler
testSendIRQDown queue irq ctx _ =
    sendCpuMsgIO queue (PCmdInterruptDown irq) >> return ctx

-------------------------------------------------------------------------------

testPC instrList = do
    describe "Test PC PIC" $ do
        let devR = 0
        it "Init PIC" $ do
            comm <- newPrismComm False
            let devices = createPC
                testHandler int val = 89
                intList = [
                    (PrismInt 0x10, testInterruptHandler testHandler)
                    ]
            env <- createPeripheralsTestEnv instrList devR [] [] devices pcPorts [] intList
            execPrismHalt [(al `shouldEq` 89)] env comm $ [text|
                mov al, 134
                int 0x10
                hlt
            |]
        it "Init PIC2" $ do
            comm <- newPrismComm False
            let devices = createPC
                testHandler int val = 89
                intList = [
                    (PrismInt 0x11, testSendIRQUp (commCmdQueue comm) (PrismIRQ 0)),
                    (PrismInt 0x20, testInterruptHandler testHandler)
                    ]
            env <- createPeripheralsTestEnv instrList devR [] [] devices pcPorts [] intList
            execPrismHalt [(al `shouldEq` 89)] env comm $ [text|
                PIC1  equ  0x20
                PIC1D equ  0x21
                ICW1  equ  0x17
                ICW2  equ  0x20
                ICW3  equ  0x00
                ICW4  equ  0x03
                OCW1  equ  0x00
                mov al, ICW1
                out PIC1, al
                mov al, ICW2
                out PIC1D, al
                mov al, ICW3
                out PIC1D, al
                mov al, ICW4
                out PIC1D, al
                mov al, OCW1
                out PIC1D, al
                mov bl, 0
                sti
                int 0x11
                mov cx, 10
                LOOP1:
                inc bl
                loop LOOP1
                hlt
            |]

-------------------------------------------------------------------------------
