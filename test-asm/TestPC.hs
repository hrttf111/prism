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
        it "Interrupt test infra" $ do
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
        it "Basic test Master PIC IRQ0" $ do
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
                ICW2  equ  0x20 ; Master interrupts 0x20-0x27
                ICW3  equ  0x00
                ICW4  equ  0x03
                OCW1  equ  0x00
                ;Init Master
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
                ;Send IRQ0
                mov bl, 0
                sti
                int 0x11
                mov cx, 10
                LOOP1:
                inc bl
                loop LOOP1
                hlt
            |]
        it "Basic test Slave PIC IRQ8" $ do
            comm <- newPrismComm False
            let devices = createPC
                testHandler int val = 89
                intList = [
                    (PrismInt 0x11, testSendIRQUp (commCmdQueue comm) (PrismIRQ 8)),
                    (PrismInt 0x28, testInterruptHandler testHandler)
                    ]
            env <- createPeripheralsTestEnv instrList devR [] [] devices pcPorts [] intList
            execPrismHalt [(al `shouldEq` 89)] env comm $ [text|
                PIC1  equ  0x20
                PIC1D equ  0x21
                PIC2  equ  0xA0
                PIC2D equ  0xA1
                ICW1  equ  0x17
                ICW2  equ  0x20 ; Master interrupts 0x20-0x27
                ICW2S equ  0x28 ; Slave interrupts 0x28-0x2F
                ICW3  equ  0x00
                ICW4  equ  0x03
                OCW1  equ  0x00
                ;Init Master
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
                ;Init Slave
                mov al, ICW1
                out PIC2, al
                mov al, ICW2S
                out PIC2D, al
                mov al, ICW3
                out PIC2D, al
                mov al, ICW4
                out PIC2D, al
                mov al, OCW1
                out PIC2D, al
                ;Send IRQ8
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
