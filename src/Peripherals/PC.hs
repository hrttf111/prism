{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Peripherals.PC where

import Control.Monad.Trans

import Data.IORef
import Data.Bits

import Prism
import PrismPeripheral
import Peripherals.Local
import Peripherals.Pic

-------------------------------------------------------------------------------

data PC = PC {
        pcCycles :: Int,
        pcNeedUpdate :: Bool,
        pcPicMaster :: Pic,
        pcPicSlave :: Pic
    } deriving (Show)

type PeripheralsPC = PeripheralsLocal PC

instance InterruptDispatcher PeripheralsPC where
    dispatchInterruptUp peripherals (PrismInt irq) = do
        pc <- readIORef (localPeripherals peripherals)
        if irq < 8 then do
            let pic = picRaiseIrq (pcPicMaster pc) irq
            pc_ <- pcPicUpdateMaster pc pic True
            writeIORef (localPeripherals peripherals) pc_
            return (peripherals, pcIntrUp pc_)
            else do
                let pic = picRaiseIrq (pcPicSlave pc) irq
                pc_ <- pcPicUpdateSlave pc pic True
                writeIORef (localPeripherals peripherals) pc_
                return (peripherals, pcIntrUp pc_)
    dispatchInterruptDown peripherals (PrismInt irq) = do
        pc <- readIORef (localPeripherals peripherals)
        if irq < 8 then do
            let pic = picLowerIrq (pcPicMaster pc) irq
            pc_ <- pcPicUpdateMaster pc pic True
            writeIORef (localPeripherals peripherals) pc_
            return (peripherals, pcIntrUp pc_)
            else do
                let pic = picLowerIrq (pcPicSlave pc) irq
                pc_ <- pcPicUpdateSlave pc pic True
                writeIORef (localPeripherals peripherals) pc_
                return (peripherals, pcIntrUp pc_)
    ackInterrupt peripherals = do
        pc <- readIORef (localPeripherals peripherals)
        let (picMaster, int) = picAck $ pcPicMaster pc
        if int == 2 then do
            let (picSlave, intS) = picAck $ pcPicSlave pc
            pc_ <- pcPicUpdateSlave pc picSlave True
            pc__ <- pcPicUpdateMaster pc_ picMaster True
            writeIORef (localPeripherals peripherals) pc__
            return (peripherals, picGetPrismInt picSlave intS)
        else do
            pc_ <- pcPicUpdateMaster pc picMaster True
            writeIORef (localPeripherals peripherals) pc_
            return (peripherals, picGetPrismInt picMaster int)

instance PeripheralRunner PeripheralsPC where
    runPeripherals ctx peripherals = do
        pc <- readIORef (localPeripherals peripherals)
        let interrupts = ctxInterrupts ctx
            intOn = (intIntrOn interrupts) && pcIntrUp pc
            interrupts_ = interrupts { intIntrOn = intOn }
            pc_ = pc { pcNeedUpdate = False }
        writeIORef (localPeripherals peripherals) pc_
        return (ctx { ctxInterrupts = interrupts_ } , peripherals)
    peripheralCycles peripherals =
        readIORef (localPeripherals peripherals) >>= return . pcCycles
    needUpdate peripherals =
        readIORef (localPeripherals peripherals) >>= return . pcNeedUpdate

-------------------------------------------------------------------------------

pcIntrUp :: PC -> Bool
pcIntrUp = picStateIntr . picState . pcPicMaster

pcPicUpdateMaster :: PC -> Pic -> Bool -> IO PC
pcPicUpdateMaster pc pic update =
    if update then
        case picUpdate pic of
            (pic_, PicIntrActive active) ->
                return pc { pcPicMaster = pic_, pcNeedUpdate = True }
            (pic_, _) ->
                return pc { pcPicMaster = pic_, pcNeedUpdate = False }
        else 
            return pc { pcPicMaster = pic , pcNeedUpdate = False }

pcPicUpdateSlave :: PC -> Pic -> Bool -> IO PC
pcPicUpdateSlave pc pic update =
    if update then
        case picUpdate pic of
            (pic_, PicIntrActive active) ->
                let picMaster = if active then picRaiseIrq (pcPicMaster pc) 2
                                    else picLowerIrq (pcPicMaster pc) 2
                    in
                pcPicUpdateMaster (pc { pcPicSlave = pic_ }) picMaster True
            (pic_, _) ->
                return pc { pcPicSlave = pic_, pcNeedUpdate = False  }
        else 
            return pc { pcPicSlave = pic, pcNeedUpdate = False }

pcPortRead8PicDataMaster :: PC -> Uint16 -> IO (PC, Uint8)
pcPortRead8PicDataMaster pc port =
    let (pic, update, val) = picReadData $ pcPicMaster pc
        in
    pcPicUpdateMaster pc pic update >>= \pc_ -> return (pc_, val)

pcPortWrite8PicDataMaster :: PC -> Uint16 -> Uint8 -> IO PC
pcPortWrite8PicDataMaster pc port val =
    let commands = picDecodeData val $ picInitStage . picState $ pcPicMaster pc
        (pic, update) = picWriteCommands (pcPicMaster pc) commands
        in
    pcPicUpdateMaster pc pic update

pcPortRead8PicControlMaster :: PC -> Uint16 -> IO (PC, Uint8)
pcPortRead8PicControlMaster pc port =
    let (pic, update, val) = picReadControl $ pcPicMaster pc
        in
    pcPicUpdateMaster pc pic update >>= \pc_ -> return (pc_, val)

pcPortWrite8PicControlMaster :: PC -> Uint16 -> Uint8 -> IO PC
pcPortWrite8PicControlMaster pc port val =
    let commands = picDecodeCommand val $ pcPicMaster pc
        (pic, update) = picWriteCommands (pcPicMaster pc) commands
        in
    pcPicUpdateMaster pc pic update

pcPortRead8PicDataSlave :: PC -> Uint16 -> IO (PC, Uint8)
pcPortRead8PicDataSlave pc port =
    let (pic, update, val) = picReadData $ pcPicSlave pc
        in
    pcPicUpdateSlave pc pic update >>= \pc_ -> return (pc_, val)

pcPortWrite8PicDataSlave :: PC -> Uint16 -> Uint8 -> IO PC
pcPortWrite8PicDataSlave pc port val =
    let commands = picDecodeData val $ picInitStage . picState $ pcPicSlave pc
        (pic, update) = picWriteCommands (pcPicSlave pc) commands
        in
    pcPicUpdateSlave pc pic update

pcPortRead8PicControlSlave :: PC -> Uint16 -> IO (PC, Uint8)
pcPortRead8PicControlSlave pc port =
    let (pic, update, val) = picReadControl $ pcPicSlave pc
        in
    pcPicUpdateSlave pc pic update >>= \pc_ -> return (pc_, val)

pcPortWrite8PicControlSlave :: PC -> Uint16 -> Uint8 -> IO PC
pcPortWrite8PicControlSlave pc port val =
    let commands = picDecodeCommand val $ pcPicSlave pc
        (pic, update) = picWriteCommands (pcPicSlave pc) commands
        in
    pcPicUpdateSlave pc pic update


pcPorts = [
        PeripheralPort 0x20
            (PeripheralHandlerPort pcPortWrite8PicDataMaster emptyWriteH pcPortRead8PicDataMaster emptyReadH),
        PeripheralPort 0x21
            (PeripheralHandlerPort pcPortWrite8PicControlMaster emptyWriteH pcPortRead8PicControlMaster emptyReadH),
        PeripheralPort 0xA0
            (PeripheralHandlerPort pcPortWrite8PicDataSlave emptyWriteH pcPortRead8PicDataSlave emptyReadH),
        PeripheralPort 0xA1
            (PeripheralHandlerPort pcPortWrite8PicControlSlave emptyWriteH pcPortRead8PicControlSlave emptyReadH)
    ]

createPC :: PC
createPC = PC 0 False defaultPIC defaultPIC

-------------------------------------------------------------------------------
