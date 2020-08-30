{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestPC where

import Test.Hspec

import Data.Text (append)
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Concurrent

import Prism.Cpu
import Prism.Command
import Prism.Peripherals
import Prism.PC

import TestAsm.Common
import TestAsm.Run

import NeatInterpolation

-------------------------------------------------------------------------------

instance PeripheralsTestCreator PeripheralsPC PC where
    createTestPeripherals (PeripheralLocal maxPorts maxMem portRegion memRegion ports mem devices) queue =
        IOCtx (PeripheralsLocal maxPorts maxMem ports mem queue emptyScheduler devices) memRegion portRegion

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

testScheduleWrite :: Uint16 -> Uint8 -> PeripheralsPC ()
testScheduleWrite _ val = do
    let handler _ = liftIO $ putStrLn "Sched event"
    localSchedulerAdd (SchedId 1) (CpuCycles $ fromIntegral val) handler
    liftIO $ putStrLn $ "Do scheduling " ++ (show val)

-------------------------------------------------------------------------------

headerPit = [text|
        %macro set_int 2
            mov ax, 0
            mov es, ax
            mov bx, cs
            mov [es:%1], WORD %2
            mov [es:%1+2], bx
        %endmacro
        %macro set_cmd 1
            push ax
            mov al, %1
            out PIT_REG_COMMAND, al
            pop ax
        %endmacro
        %macro set_ctr 3
            push ax
            mov al, %2 ; least
            out %1, al
            mov al, %3 ; most
            out %1, al
            pop ax
        %endmacro
        %macro read_ctr 2
            push ax
            in al, %1
            mov %2, al
            pop ax
        %endmacro
        absolute 0x80
            interrupt_1   resw    2
        section .text
        org 0
            ;;;
            PIC1  equ  0x20
            PIC1D equ  0x21
            ICW1  equ  0x17
            ICW2  equ  0x20 ; Map IRQ 0 to INT 0x20
            ICW3  equ  0x00
            ICW4  equ  0x03
            OCW1  equ  0x00
            ;;;;
            PIT_REG_COUNTER0 equ 0x40
            PIT_REG_COUNTER1 equ 0x41
            PIT_REG_COUNTER2 equ 0x42
            PIT_REG_COMMAND  equ 0x43
            PIT_INT_TIMER0   equ 0x80
            ;Init Master PIC
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
    |]

-------------------------------------------------------------------------------

emptyMemR :: [PeripheralMem (RemoteTrans Int)]
emptyMemR = []

emptyPortR :: [PeripheralPort (RemoteTrans Int)]
emptyPortR = []

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
            env <- createPeripheralsTestEnv instrList devR emptyPortR emptyMemR devices pcPorts [] intList
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
            env <- createPeripheralsTestEnv instrList devR emptyPortR emptyMemR devices pcPorts [] intList
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
            env <- createPeripheralsTestEnv instrList devR emptyPortR emptyMemR devices pcPorts [] intList
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
    describe "Test PC Scheduler" $ do
        it "Sched" $ do
            comm <- newPrismComm False
            let devices = createPC
                devR = 0
                pcPortsExt = pcPorts ++ [
                        PeripheralPort 0x89
                            (PeripheralHandlerPort testScheduleWrite emptyWriteH emptyReadH emptyReadH)
                    ]
                intList = []
            env <- createPeripheralsTestEnv instrList devR emptyPortR emptyMemR devices pcPortsExt [] intList
            execPrismHalt [(al `shouldEq` 5)] env comm $ [text|
                mov al, 5
                out 0x89, al
                mov cx, 10
                LOOP1:
                inc bl
                loop LOOP1
                hlt
            |]
    describe "Test PC PIT" $ do
        let devR = 0
        it "Write, read and interrupt Timer 0" $ do
            comm <- newPrismComm False
            let devices = createPC
                expectedStatus = 0xF0
            env <- createPeripheralsTestEnv instrList devR emptyPortR emptyMemR devices pcPorts [] []
            execPrismHalt [(bx `shouldEq` 51), (cl `shouldEq` expectedStatus), (dx `shouldEq` 1)] env comm $ append headerPit [text|
                ;Init PIT
                PIT_READ_STATUS  equ 0xE2 ; ReadBack Timer0, Status
                PIT_COMMAND      equ 0x30 ; Timer0, Mode0, 2 Bytes, HEX
                set_int interrupt_1, INTERRUPT1
                set_cmd PIT_COMMAND
                sti
                xor bx, bx
                xor dx, dx
                mov cx, 90
                set_ctr PIT_REG_COUNTER0, 10, 0 ; 40 cycles
                ;Start test
                LOOP1: loop LOOP1
                ;Read status
                set_cmd PIT_READ_STATUS
                read_ctr PIT_REG_COUNTER0, cl
                hlt

                INTERRUPT1: 
                mov bx, cx
                inc dx
                iret
            |]
        it "Write, read and interrupt Timer 2" $ do
            comm <- newPrismComm False
            let devices = createPC
                expectedStatus = 0xF4
            env <- createPeripheralsTestEnv instrList devR emptyPortR emptyMemR devices pcPorts [] []
            execPrismHalt [(bx `shouldEq` 21), (cl `shouldEq` expectedStatus), (dx `shouldEq` 4)] env comm $ append headerPit [text|
                ;Init PIT
                PIT_READ_STATUS  equ 0xE2 ; ReadBack Timer0, Status
                PIT_COMMAND      equ 0x34 ; Timer0, Mode2, 2 Bytes, HEX
                set_int interrupt_1, INTERRUPT1
                sti
                xor bx, bx
                xor dx, dx
                mov cx, 130
                set_cmd PIT_COMMAND
                set_ctr PIT_REG_COUNTER0, 10, 0 ; 40 cycles
                ;Start test
                LOOP1: loop LOOP1
                ;Read status
                set_cmd PIT_READ_STATUS
                read_ctr PIT_REG_COUNTER0, cl
                hlt

                INTERRUPT1: 
                mov bx, cx
                inc dx
                iret
            |]
        it "Write, read and interrupt Timer 4" $ do
            comm <- newPrismComm False
            let devices = createPC
                expectedStatus = 0xF8
            env <- createPeripheralsTestEnv instrList devR emptyPortR emptyMemR devices pcPorts [] []
            execPrismHalt [(bx `shouldEq` 91), (cl `shouldEq` expectedStatus), (dx `shouldEq` 2)] env comm $ append headerPit [text|
                ;Init PIT
                PIT_READ_STATUS  equ 0xE2 ; ReadBack Timer0, Status
                PIT_COMMAND      equ 0x38 ; Timer0, Mode4, 2 Bytes, HEX
                set_int interrupt_1, INTERRUPT1
                sti
                xor bx, bx
                xor dx, dx
                mov cx, 130
                set_cmd PIT_COMMAND
                set_ctr PIT_REG_COUNTER0, 10, 0 ; 40 cycles
                ;Start test
                LOOP1: loop LOOP1
                ;Read status
                set_cmd PIT_READ_STATUS
                read_ctr PIT_REG_COUNTER0, cl
                hlt

                INTERRUPT1: 
                mov bx, cx
                inc dx
                iret
            |]
-------------------------------------------------------------------------------
