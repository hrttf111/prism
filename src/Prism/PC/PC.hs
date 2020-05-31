{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prism.PC.PC where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

import Data.Bits

import Prism.Cpu
import Prism.Peripherals
import Prism.PC.Pic

-------------------------------------------------------------------------------

data PC = PC {
        pcCycles :: Int,
        pcNeedUpdate :: Bool,
        pcPicMaster :: Pic,
        pcPicSlave :: Pic
    } deriving (Show)

type PeripheralsPC' = PeripheralsLocal PC
type PeripheralsPC = LocalTrans PC

instance InterruptDispatcher PeripheralsPC where
    dispatchIrqUp (PrismIRQ irq) = do
        pc <- localPeripherals <$> get
        if irq < 8 then do
            let pic = picRaiseIrq (pcPicMaster pc) irq
            pc_ <- pcPicUpdateMaster pc pic True
            modify (\s -> s { localPeripherals = pc_ })
            return $ pcIntrUp pc_
            else do
                let pic = picRaiseIrq (pcPicSlave pc) irq
                pc_ <- pcPicUpdateSlave pc pic True
                modify (\s -> s { localPeripherals = pc_ })
                return $ pcIntrUp pc_
    dispatchIrqDown (PrismIRQ irq) = do
        pc <- localPeripherals <$> get
        if irq < 8 then do
            let pic = picLowerIrq (pcPicMaster pc) irq
            pc_ <- pcPicUpdateMaster pc pic True
            modify (\s -> s { localPeripherals = pc_ })
            return $ pcIntrUp pc_
            else do
                let pic = picLowerIrq (pcPicSlave pc) irq
                pc_ <- pcPicUpdateSlave pc pic True
                modify (\s -> s { localPeripherals = pc_ })
                return $ pcIntrUp pc_
    ackIrq = do
        pc <- localPeripherals <$> get
        let (picMaster, int) = picAck $ pcPicMaster pc
        if int == 2 then do
            let (picSlave, intS) = picAck $ pcPicSlave pc
            pc_ <- pcPicUpdateSlave pc picSlave True
            pc__ <- pcPicUpdateMaster pc_ picMaster True
            modify (\s -> s { localPeripherals = pc__ })
            return $ picGetPrismInt picSlave intS
        else do
            pc_ <- pcPicUpdateMaster pc picMaster True
            modify (\s -> s { localPeripherals = pc_ })
            return $ picGetPrismInt picMaster int

instance PeripheralsMonad PeripheralsPC where
    runPeripherals = return ()
    nextInstrTime = return 0

instance RunPeripheralsM PeripheralsPC' PeripheralsPC PrismM where
    runPeripheralsM ctx c = do
        cpuCtx <- get
        (res, pcCtx) <- liftIO $ ((runStateT . runLocal $ c) ctx)
        let c1 = ctxIO cpuCtx
            (pTime, sched') = reschedule (localScheduler pcCtx) (SchedTime $ ctxCycles cpuCtx)
            (cpuCtx', pcCtx') = processInterrupts cpuCtx $ pcCtx { localScheduler = sched' }
            ioCtx = IOCtx pcCtx'
                          (ioCtxMemRegion c1)
                          (ioCtxPortRegion c1)
            cyclesP = maybe 9999999999 (\(SchedTime t) -> t) pTime
        put $ cpuCtx' { ctxIO = ioCtx, ctxCyclesP = cyclesP }
        return res
        where
            processInterrupts :: Ctx -> PeripheralsPC' -> (Ctx, PeripheralsPC')
            processInterrupts cpuCtx pcCtx =
                let pc = localPeripherals pcCtx
                    in
                if pcNeedUpdate pc then
                    let pc' = pc { pcNeedUpdate = False }
                        pcCtx' = pcCtx { localPeripherals = pc' }
                        interrupts = ctxInterrupts cpuCtx
                        intOn = pcIntrUp pc
                        interruptUp = (intInterruptUp interrupts) || intOn
                        interrupts_ = interrupts { intIntrOn = intOn, intInterruptUp = interruptUp}
                        cpuCtx' = cpuCtx { ctxInterrupts = interrupts_ }
                        in
                        (cpuCtx', pcCtx')
                    else
                        (cpuCtx, pcCtx)

-------------------------------------------------------------------------------

pcIntrUp :: PC -> Bool
pcIntrUp =
    picStateIntr . picState . pcPicMaster

pcPicUpdateMaster :: (MonadIO m) => PC -> Pic -> Bool -> m PC
pcPicUpdateMaster pc pic update =
    if update then
        case picUpdate pic of
            (pic_, PicIntrActive active) ->
                return pc { pcPicMaster = pic_, pcNeedUpdate = active }
            (pic_, _) ->
                return pc { pcPicMaster = pic_, pcNeedUpdate = False }
        else 
            return pc { pcPicMaster = pic , pcNeedUpdate = False }

pcPicUpdateSlave :: (MonadIO m) => PC -> Pic -> Bool -> m PC
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

-------------------------------------------------------------------------------

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
            (PeripheralHandlerPort pcPortWrite8PicControlMaster emptyWriteH pcPortRead8PicControlMaster emptyReadH),
        PeripheralPort 0x21
            (PeripheralHandlerPort pcPortWrite8PicDataMaster emptyWriteH pcPortRead8PicDataMaster emptyReadH),
        PeripheralPort 0xA0
            (PeripheralHandlerPort pcPortWrite8PicControlSlave emptyWriteH pcPortRead8PicControlSlave emptyReadH),
        PeripheralPort 0xA1
            (PeripheralHandlerPort pcPortWrite8PicDataSlave emptyWriteH pcPortRead8PicDataSlave emptyReadH)
    ]

createPC :: PC
createPC = PC 999 False defaultPIC defaultPIC

-------------------------------------------------------------------------------
