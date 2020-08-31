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
import Prism.PC.Pit

-------------------------------------------------------------------------------

data PC = PC {
        pcCycles :: CpuCycles,
        pcNeedUpdate :: Bool,
        pcPicMaster :: Pic,
        pcPicSlave :: Pic,
        pcPit :: Pit
    } deriving (Show)

type PeripheralsPC' = PeripheralsLocal PC
type PeripheralsPC = LocalTrans PC

instance InterruptDispatcher PeripheralsPC where
    dispatchIrqUp (PrismIRQ irq) = do
        pc <- getPC
        if irq < 8 then do
            let pic = picRaiseIrq (pcPicMaster pc) irq
            putPC $ pcPicUpdateMaster pc pic True
            getPCIntrUp
            else do
                let pic = picRaiseIrq (pcPicSlave pc) irq
                putPC $ pcPicUpdateSlave pc pic True
                getPCIntrUp
    dispatchIrqDown (PrismIRQ irq) = do
        pc <- getPC
        if irq < 8 then do
            let pic = picLowerIrq (pcPicMaster pc) irq
            putPC $ pcPicUpdateMaster pc pic True
            getPCIntrUp
            else do
                let pic = picLowerIrq (pcPicSlave pc) irq
                putPC $ pcPicUpdateSlave pc pic True
                getPCIntrUp
    ackIrq = do
        pc <- getPC
        let (picMaster, int) = picAck $ pcPicMaster pc
        if int == 2 then do
            let (picSlave, intS) = picAck $ pcPicSlave pc
                pc_ = pcPicUpdateSlave pc picSlave True
                pc__ = pcPicUpdateMaster pc_ picMaster True
            putPC pc__
            return $ picGetPrismInt picSlave intS
        else do
            putPC $ pcPicUpdateMaster pc picMaster True
            return $ picGetPrismInt picMaster int

instance PeripheralsMonad PeripheralsPC where
    runPeripherals = do
        pc <- localPeripherals <$> get
        let currentTime = pcCycles pc
        events <- localSchedulerExpired currentTime
        mapM_ id events

instance RunPeripheralsM PeripheralsPC' PeripheralsPC PrismM where
    runPeripheralsM ctx actions = do
        cpuCtx <- get
        ((cyclesP, res), pcCtx) <- liftIO $ (runStateT . runLocal $ pcActions) $ setCycles cpuCtx ctx
        let c1 = ctxIO cpuCtx
            (cpuCtx', pcCtx') = processInterrupts cpuCtx $ pcCtx
            ioCtx = IOCtx pcCtx'
                          (ioCtxMemRegion c1)
                          (ioCtxPortRegion c1)
        put $ cpuCtx' { ctxIO = ioCtx, ctxCyclesP = cyclesP }
        return res
        where
            pcActions = do
                v <- actions
                pc <- localPeripherals <$> get
                pCycles <- localSchedulerReschedule $ pcCycles pc
                return (pCycles, v)
            setCycles :: Ctx -> PeripheralsPC' -> PeripheralsPC'
            setCycles ctx pcCtx =
                let pc = localPeripherals pcCtx
                    pc' = pc { pcCycles = ctxCycles ctx}
                    in
                pcCtx { localPeripherals = pc' }
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

getPC :: PeripheralsPC PC
getPC = localPeripherals <$> get

putPC :: PC -> PeripheralsPC ()
putPC pc = modify $ \s -> s { localPeripherals = pc }

getPCIntrUp :: PeripheralsPC Bool
getPCIntrUp = pcIntrUp <$> getPC

-------------------------------------------------------------------------------

pcIntrUp :: PC -> Bool
pcIntrUp =
    picStateIntr . picState . pcPicMaster

pcPicUpdateMaster :: PC -> Pic -> Bool -> PC
pcPicUpdateMaster pc pic update =
    if update then
        case picUpdate pic of
            (pic_, PicIntrActive active) ->
                pc { pcPicMaster = pic_, pcNeedUpdate = active }
            (pic_, _) ->
                pc { pcPicMaster = pic_, pcNeedUpdate = False }
        else 
            pc { pcPicMaster = pic , pcNeedUpdate = False }

pcPicUpdateSlave :: PC -> Pic -> Bool -> PC
pcPicUpdateSlave pc pic update =
    if update then
        case picUpdate pic of
            (pic_, PicIntrActive active) ->
                let picMaster = if active then picRaiseIrq (pcPicMaster pc) 2
                                    else picLowerIrq (pcPicMaster pc) 2
                    in
                pcPicUpdateMaster (pc { pcPicSlave = pic_ }) picMaster True
            (pic_, _) ->
                pc { pcPicSlave = pic_, pcNeedUpdate = False  }
        else 
            pc { pcPicSlave = pic, pcNeedUpdate = False }

-------------------------------------------------------------------------------

pcPortRead8PicDataMaster :: Uint16 -> PeripheralsPC Uint8
pcPortRead8PicDataMaster port = do
    pc <- getPC
    let (pic, update, val) = picReadData $ pcPicMaster pc
    putPC $ pcPicUpdateMaster pc pic update
    return val

pcPortWrite8PicDataMaster :: Uint16 -> Uint8 -> PeripheralsPC ()
pcPortWrite8PicDataMaster port val = do
    pc <- getPC
    let commands = picDecodeData val $ picInitStage . picState $ pcPicMaster pc
        (pic, update) = picWriteCommands (pcPicMaster pc) commands
    putPC $ pcPicUpdateMaster pc pic update

pcPortRead8PicControlMaster :: Uint16 -> PeripheralsPC Uint8
pcPortRead8PicControlMaster port = do
    pc <- getPC
    let (pic, update, val) = picReadControl $ pcPicMaster pc
    putPC $ pcPicUpdateMaster pc pic update
    return val

pcPortWrite8PicControlMaster :: Uint16 -> Uint8 -> PeripheralsPC ()
pcPortWrite8PicControlMaster port val = do
    pc <- getPC
    let commands = picDecodeCommand val $ pcPicMaster pc
        (pic, update) = picWriteCommands (pcPicMaster pc) commands
    putPC $ pcPicUpdateMaster pc pic update

pcPortRead8PicDataSlave :: Uint16 -> PeripheralsPC Uint8
pcPortRead8PicDataSlave port = do
    pc <- getPC
    let (pic, update, val) = picReadData $ pcPicSlave pc
    putPC $ pcPicUpdateSlave pc pic update
    return val

pcPortWrite8PicDataSlave :: Uint16 -> Uint8 -> PeripheralsPC ()
pcPortWrite8PicDataSlave port val = do
    pc <- getPC
    let commands = picDecodeData val $ picInitStage . picState $ pcPicSlave pc
        (pic, update) = picWriteCommands (pcPicSlave pc) commands
    putPC $ pcPicUpdateSlave pc pic update

pcPortRead8PicControlSlave :: Uint16 -> PeripheralsPC Uint8
pcPortRead8PicControlSlave port = do
    pc <- getPC
    let (pic, update, val) = picReadControl $ pcPicSlave pc
    putPC $ pcPicUpdateSlave pc pic update
    return val

pcPortWrite8PicControlSlave :: Uint16 -> Uint8 -> PeripheralsPC ()
pcPortWrite8PicControlSlave port val = do
    pc <- getPC
    let commands = picDecodeCommand val $ pcPicSlave pc
        (pic, update) = picWriteCommands (pcPicSlave pc) commands
    putPC $ pcPicUpdateSlave pc pic update

-------------------------------------------------------------------------------

pcPitUpdate :: Pit -> PeripheralsPC ()
pcPitUpdate pit = do
    currentCycles <- pcCycles <$> getPC
    let (pit', actions) = pitDoUpdate pit currentCycles
    foldM_ (\ _ action -> do
            liftIO $ putStrLn $ show action
            case action of
                PitActionIrq True irq -> do
                    dispatchIrqUp irq
                    return ()
                PitActionIrq False irq -> do
                    dispatchIrqDown irq
                    return ()
                PitActionScheduleAdd schedId cycles -> do
                    localSchedulerRemove schedId
                    localSchedulerAdd schedId cycles pcEventHandlerPit
                PitActionScheduleRemove schedId ->
                    localSchedulerRemove schedId
            return ()
           ) () actions
    pc <- getPC
    putPC $ pc { pcPit = pit' }

pcPortWritePit :: Uint8 -> PitCounterNum -> PeripheralsPC ()
pcPortWritePit val counter = do
    pc <- getPC
    pcPitUpdate $ pitWrite (pcPit pc) counter (pcCycles pc) val

pcPortReadPit :: PitCounterNum -> PeripheralsPC Uint8
pcPortReadPit counter = do
    pc <- getPC
    let (val, pit) = pitRead (pcPit pc) counter (pcCycles pc)
    putPC $ pc { pcPit = pit }
    return val

pcPortWrite8PitCommand :: Uint16 -> Uint8 -> PeripheralsPC ()
pcPortWrite8PitCommand port val = do
    pc <- getPC
    pcPitUpdate $ pitControlCommand (pcPit pc) (pcCycles pc) val

pcPortWrite8PitTimer0 :: Uint16 -> Uint8 -> PeripheralsPC ()
pcPortWrite8PitTimer0 port val =
    pcPortWritePit val PitCounterNum0

pcPortRead8PitTimer0 :: Uint16 -> PeripheralsPC Uint8
pcPortRead8PitTimer0 port =
    pcPortReadPit PitCounterNum0

pcPortWrite8PitTimer1 :: Uint16 -> Uint8 -> PeripheralsPC ()
pcPortWrite8PitTimer1 port val =
    pcPortWritePit val PitCounterNum1

pcPortRead8PitTimer1 :: Uint16 -> PeripheralsPC Uint8
pcPortRead8PitTimer1 port =
    pcPortReadPit PitCounterNum1

pcPortWrite8PitTimer2 :: Uint16 -> Uint8 -> PeripheralsPC ()
pcPortWrite8PitTimer2 port val =
    pcPortWritePit val PitCounterNum2

pcPortRead8PitTimer2 :: Uint16 -> PeripheralsPC Uint8
pcPortRead8PitTimer2 port =
    pcPortReadPit PitCounterNum2

pcEventHandlerPit :: SchedHandler PeripheralsPC
pcEventHandlerPit schedId = do
    pc <- getPC
    pcPitUpdate $ pitSetEvent (pcPit pc) PitCounterNum0 (pcCycles pc)

-------------------------------------------------------------------------------

pcPorts = [
        PeripheralPort 0x20
            (PeripheralHandlerPort pcPortWrite8PicControlMaster emptyWriteH pcPortRead8PicControlMaster emptyReadH),
        PeripheralPort 0x21
            (PeripheralHandlerPort pcPortWrite8PicDataMaster emptyWriteH pcPortRead8PicDataMaster emptyReadH),
        PeripheralPort 0xA0
            (PeripheralHandlerPort pcPortWrite8PicControlSlave emptyWriteH pcPortRead8PicControlSlave emptyReadH),
        PeripheralPort 0xA1
            (PeripheralHandlerPort pcPortWrite8PicDataSlave emptyWriteH pcPortRead8PicDataSlave emptyReadH),
        PeripheralPort 0x40
            (PeripheralHandlerPort pcPortWrite8PitTimer0 emptyWriteH pcPortRead8PitTimer0 emptyReadH),
        PeripheralPort 0x41
            (PeripheralHandlerPort pcPortWrite8PitTimer1 emptyWriteH pcPortRead8PitTimer1 emptyReadH),
        PeripheralPort 0x42
            (PeripheralHandlerPort pcPortWrite8PitTimer2 emptyWriteH pcPortRead8PitTimer2 emptyReadH),
        PeripheralPort 0x43
            (PeripheralHandlerPort pcPortWrite8PitCommand emptyWriteH emptyReadH emptyReadH)
    ]

createPC :: PC
createPC = PC 999 False defaultPIC defaultPIC defaultPIT

-------------------------------------------------------------------------------
