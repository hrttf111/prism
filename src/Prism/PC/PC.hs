{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prism.PC.PC where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict
import Control.Concurrent.STM (TVar, TMVar)

import Data.Bits
import qualified Data.Map.Strict as Map
import Numeric

import Prism.Cpu
import Prism.Peripherals
import Prism.PC.Pic
import Prism.PC.Pit
import Prism.PC.Bios
import qualified Prism.Log as Log

-------------------------------------------------------------------------------

data PC = PC {
        pcCycles :: CpuCycles,
        pcNeedUpdate :: Bool,
        pcPicMaster :: Pic,
        pcPicSlave :: Pic,
        pcPit :: Pit,
        pcBios :: PcBios,
        pcHalt :: Bool
    } deriving (Show)

type PeripheralsPC' = PeripheralsLocal PC
type PeripheralsPC = LocalTrans PC

instance InterruptDispatcher PeripheralsPC where
    dispatchIrqUp (PrismIRQ irq) = do
        Log.cpuLogT Debug Log.PrismPc $ "dispatchIrqUp " ++ show irq
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
        Log.cpuLogT Debug Log.PrismPc $ "dispatchIrqDown " ++ show irq
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
        Log.cpuLogT Debug Log.PrismPc $ "ackIrq " ++ show int
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
        Log.cpuLogT Debug Log.PrismPc $ "Ints: " ++ show (ctxInterrupts cpuCtx)
        ((cyclesP, res), pcCtx) <- liftIO $ (runStateT . runLocal $ pcActions) $ setCycles cpuCtx ctx
        let c1 = ctxIO cpuCtx
            (cpuCtx', pcCtx') = processInterrupts cpuCtx pcCtx
            ioCtx = IOCtx pcCtx'
                          (ioCtxMemRegion c1)
                          (ioCtxPortRegion c1)
            doHalt = pcHalt $ localPeripherals pcCtx
        flagIF' <- getFlag IF
        Log.cpuLogT Debug Log.PrismPc $ "Master: " ++ (show $ pcPicMaster $ localPeripherals $ pcCtx)
        Log.cpuLogT Debug Log.PrismPc $ "Master after: " ++ (show $ pcPicMaster $ localPeripherals $ pcCtx')
        Log.cpuLogT Debug Log.PrismPc $ "intOn: " ++ (show $ pcIntrUp $ localPeripherals $ pcCtx)
        Log.cpuLogT Debug Log.PrismPc $ "intOn after: " ++ (show $ pcIntrUp $ localPeripherals $ pcCtx')
        Log.cpuLogT Debug Log.PrismPc $ "Cycles: " ++ show cyclesP ++ ", ints=" ++ (show . ctxInterrupts $ cpuCtx') ++ ", IF=" ++ (show flagIF')
        put $ cpuCtx' { ctxIO = ioCtx, ctxCyclesP = cyclesP }
        when doHalt $ do
            Log.cpuLogT Warning Log.PrismPc "HALT"
            cpuHalt
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
                if pcNeedUpdate pc || pcIntrUp pc then
                    let pc' = pc { pcNeedUpdate = False }
                        pcCtx' = pcCtx { localPeripherals = pc' }
                        interrupts = ctxInterrupts cpuCtx
                        intOn = pcIntrUp pc
                        interruptUp_ = (intInterruptUp interrupts) || intOn
                        interrupts_ = interrupts { intIntrOn = intOn, intInterruptUp = interruptUp_}
                        cpuCtx' = cpuCtx { ctxInterrupts = interrupts_ }
                        in
                        (cpuCtx', pcCtx')
                    else
                        (cpuCtx, pcCtx)

instance RunPeripheralsDirect PeripheralsPC' PrismM where
    runPeripheralsDirect ctx (DirectCommandU8 int) = do
        let pcCtx = localPeripherals ctx
            bios = pcBios pcCtx
        bios' <- processBios bios int
        cpuCtx <- get
        let pCtx' = ctx { localPeripherals = (pcCtx { pcBios = bios' }) }
            c1 = ctxIO cpuCtx
            ioCtx = IOCtx pCtx'
                          (ioCtxMemRegion c1)
                          (ioCtxPortRegion c1)
        put $ cpuCtx { ctxIO = ioCtx }
        return ()

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
            Log.cpuLogT Debug Log.PrismPc $ "PIT action: " ++ show action
            case action of
                PitActionIrq _ (PrismIRQ 100) -> do -- ignore Timer1 and Timer2
                    Log.cpuLogT Debug Log.PrismPc "Ignore IRQ 100"
                    return ()
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

pcHaltUp :: String -> MemOffset -> String -> PeripheralsPC ()
pcHaltUp s offset s2 = do
    pc <- getPC
    Log.cpuLogT Warning Log.PrismPc $ "DO HALT = " ++ s ++ "[0x" ++ (showHex offset "") ++ "]" ++ (if null s2 then "" else ("=" ++ s2))
    putPC $ pc { pcHalt = True }

pcMemoryBiosDataW8 :: MemOffset -> Uint8 -> PeripheralsPC ()
pcMemoryBiosDataW8 offset val = pcHaltUp "Write8" offset $ show val

pcMemoryBiosDataW16 :: MemOffset -> Uint16 -> PeripheralsPC ()
pcMemoryBiosDataW16 offset val = pcHaltUp "Write16" offset $ show val

pcMemoryBiosDataR8 :: MemOffset -> PeripheralsPC Uint8
pcMemoryBiosDataR8 offset = pcHaltUp "Read8" offset "" >> return 0

pcMemoryBiosDataR16 :: MemOffset -> PeripheralsPC Uint16
pcMemoryBiosDataR16 offset = pcHaltUp "Read16" offset "" >> return 0

pcBiosMemHandler offset = PeripheralMem offset
                            $ PeripheralHandlerMem pcMemoryBiosDataW8 pcMemoryBiosDataW16 pcMemoryBiosDataR8 pcMemoryBiosDataR16

pcMemoryBiosDataW8_0 :: MemOffset -> Uint8 -> PeripheralsPC ()
pcMemoryBiosDataW8_0 offset val = return ()

pcMemoryBiosDataW16_0 :: MemOffset -> Uint16 -> PeripheralsPC ()
pcMemoryBiosDataW16_0 offset val = return ()

pcMemoryBiosDataR8_0 :: MemOffset -> PeripheralsPC Uint8
pcMemoryBiosDataR8_0 offset = return 0

pcMemoryBiosDataR16_0 :: MemOffset -> PeripheralsPC Uint16
pcMemoryBiosDataR16_0 offset = return 0

pcBiosMemHandler_0 offset = PeripheralMem offset
                            $ PeripheralHandlerMem pcMemoryBiosDataW8_0 pcMemoryBiosDataW16_0 pcMemoryBiosDataR8_0 pcMemoryBiosDataR16_0

pcMemoryBiosDataW8_1 :: MemOffset -> Uint8 -> PeripheralsPC ()
pcMemoryBiosDataW8_1 offset val = do
    Log.cpuLogT Warning Log.PrismPc $ "Write [0x" ++ (showHex offset "") ++ "]" ++ show val
    --writeOp (MemPhy8Abs offset) val
    return ()

pcMemoryBiosDataW16_1 :: MemOffset -> Uint16 -> PeripheralsPC ()
pcMemoryBiosDataW16_1 offset val = do
    Log.cpuLogT Warning Log.PrismPc $ "Write [0x" ++ (showHex offset "") ++ "]" ++ show val
    --writeOp (MemPhy16Abs offset) val
    return ()

pcMemoryBiosDataR8_1 :: MemOffset -> PeripheralsPC Uint8
pcMemoryBiosDataR8_1 off = fromIntegral <$> pcMemoryBiosDataR16_1 off

pcMemoryBiosDataR16_1 :: MemOffset -> PeripheralsPC Uint16
pcMemoryBiosDataR16_1 0x44a = return 80 -- screen columns
pcMemoryBiosDataR16_1 0x46c = return 0
pcMemoryBiosDataR16_1 off = do
    Log.cpuLogT Warning Log.PrismPc $ "Read [0x" ++ (showHex off "") ++ "]"
    return 0

pcBiosMemHandler_1 offset = PeripheralMem offset
                            $ PeripheralHandlerMem pcMemoryBiosDataW8 pcMemoryBiosDataW16 pcMemoryBiosDataR8_1 pcMemoryBiosDataR16_1

--pcMemory = [ pcBiosMemHandler_1 $ MemLocation 0x400 0x500 ]
{-pcMemory = [ (pcBiosMemHandler $ MemLocation 0x400 0x470)
           , (pcBiosMemHandler_0 $ MemLocation 0x471 0x471)
           , (pcBiosMemHandler $ MemLocation 0x472 0x495)
           , (pcBiosMemHandler_0 $ MemLocation 0x496 0x496)
           , (pcBiosMemHandler $ MemLocation 0x497 0x500)
           ]-}
--pcMemory = [ pcBiosMemHandler $ MemLocation 0x400 0x500 ]
--pcMemory = [ (pcBiosMemHandler $ MemLocation 0x400 0x500), (pcBiosMemHandler $ MemLocation 3354 3354) ]
pcMemory = []

-------------------------------------------------------------------------------

pcHaltUpPort :: String -> Uint16 -> String -> PeripheralsPC ()
pcHaltUpPort s port s2 = do
    pc <- getPC
    Log.cpuLogT Warning Log.PrismPc $ "DO HALT = " ++ s ++ "[0x" ++ (showHex port "") ++ "]" ++ (if null s2 then "" else ("=" ++ s2))
    putPC $ pc { pcHalt = True }

pcPortReadF8 :: Uint16 -> PeripheralsPC Uint8
pcPortReadF8 port = pcHaltUpPort "Read port 8" port "" >> return 0

pcPortReadF16 :: Uint16 -> PeripheralsPC Uint16
pcPortReadF16 port = pcHaltUpPort "Read port 16" port "" >> return 0

pcPortWriteF8 :: Uint16 -> Uint8 -> PeripheralsPC ()
pcPortWriteF8 port val = pcHaltUpPort "Write port 8" port (show val)

pcPortWriteF16 :: Uint16 -> Uint16 -> PeripheralsPC ()
pcPortWriteF16 port val = pcHaltUpPort "Write port 16" port (show val)

pcPortHandlerF = PeripheralHandlerPort pcPortWriteF8 pcPortWriteF16 pcPortReadF8 pcPortReadF16

pcPortHandlerIgnore = PeripheralHandlerPort emptyWriteH emptyWriteH emptyReadH emptyReadH

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
            (PeripheralHandlerPort pcPortWrite8PitCommand emptyWriteH emptyReadH emptyReadH),
        PeripheralPort 0x60 pcPortHandlerF,
        PeripheralPort 0x61 pcPortHandlerIgnore, -- speaker, do nothing
        PeripheralPort 0x62 pcPortHandlerF,
        PeripheralPort 0x63 pcPortHandlerF,
        PeripheralPort 0x64 pcPortHandlerF,
        PeripheralPort 0x3B4 pcPortHandlerF,
        PeripheralPort 0x3B8 pcPortHandlerF,
        PeripheralPort 0x3B5 pcPortHandlerF,
        PeripheralPort 0x3D4 pcPortHandlerF,
        PeripheralPort 0x3D5 pcPortHandlerF,
        PeripheralPort 0x49D pcPortHandlerF,
        PeripheralPort 0x49E pcPortHandlerF,
        PeripheralPort 0x4AD pcPortHandlerF
    ]

createPC :: (MonadIO m) => m PC
createPC = do
    bios <- mkBios
    return $ PC 999 False defaultPIC defaultPIC defaultPIT bios False

createPcWithDisks :: (MonadIO m) => [(PcDiskIndex, PcDisk)] -> m PC
createPcWithDisks disks = do
    pc <- createPC
    let bios = pcBios pc
        pcDisks' = Map.fromList disks
        bios' = bios { pcDisks = pcDisks' }
    return $ pc { pcBios = bios' }

setPcMemory :: PC -> PrismM ()
setPcMemory pc = do
    setBiosMemory $ pcBios pc

rebootPc :: PC -> PrismM ()
rebootPc pc = do
    loadBootSector $ pcBios pc
    return ()

getPcBiosSharedState :: PC -> (TMVar SharedKeyboardState, TMVar SharedVideoState)
getPcBiosSharedState pc = (pcKeyboardShared . pcBiosKeyboard . pcBios $ pc, pcVideoShared . pcVideoState . pcBios $ pc)

-------------------------------------------------------------------------------
