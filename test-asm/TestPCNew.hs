{-# LANGUAGE QuasiQuotes #-}

module TestPCNew where

import Test.Hspec

import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Concurrent

import Prism.Cpu
import Prism.Command
import Prism.Peripherals
import Prism.PC

import TestAsm.Common
import TestAsm.Run

import NeatInterpolation

-------------------------------------------------------------------------------

instance PeripheralsTestCreator PC where
    createTestPeripherals (PeripheralLocal maxPorts maxMem portRegion memRegion ports mem devices) queue =
        IOCtx (PeripheralsLocal maxPorts maxMem ports mem queue devices) memRegion portRegion

type TestInterruptHandler = Uint8 -> Uint8 -> Uint8

testInterruptHandler :: TestInterruptHandler -> InterruptHandler
testInterruptHandler handler int =
    (liftIO $ putStrLn "In handler") >> (handler int <$> readOp al) >>= writeOp al

testSendIRQUp :: PrismCmdQueue -> PrismIRQ -> InterruptHandler
testSendIRQUp queue irq _ =
    sendCpuMsgIO queue (PCmdInterruptUp irq)

testSendIRQDown :: PrismCmdQueue -> PrismIRQ -> InterruptHandler
testSendIRQDown queue irq _ =
    sendCpuMsgIO queue (PCmdInterruptDown irq)

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
