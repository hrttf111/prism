{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prism.Peripherals.Local where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

import qualified Data.Array as Array

import Prism.Cpu
import qualified Prism.Log as Log

import Prism.Peripherals.Types
import Prism.Peripherals.Builder
import Prism.Peripherals.Queue
import Prism.Peripherals.Scheduler

-------------------------------------------------------------------------------

data PeripheralsLocal p = PeripheralsLocal {
        localMaxPort :: IOHandlerIndex,
        localMaxMem :: IOHandlerIndex,
        localPeripheralPort :: PeripheralArray (PeripheralHandlerPort (LocalTrans p)),
        localPeripheralMem :: PeripheralArray (PeripheralHandlerMem (LocalTrans p)),
        localIOQueue :: IOQueue,
        localScheduler :: Scheduler (LocalTrans p),
        localDebugCtx :: DebugCtx,
        localPeripherals :: p
    }

-------------------------------------------------------------------------------

newtype LocalTransM s m a = LocalTransM {
    runLocal :: (StateT s m) a
} deriving (Monad, Applicative, Functor, MonadState s)

instance MonadTrans (LocalTransM s) where
    lift = LocalTransM . lift

instance MonadIO m => MonadIO (LocalTransM s m) where
    liftIO = lift . liftIO

type LocalTrans p = LocalTransM (PeripheralsLocal p) IO

-------------------------------------------------------------------------------

printLog :: (Enum level) => level -> LogFeature -> String -> (LocalTrans p) ()
printLog level (LogFeature index) msg = do
    (DebugCtx printF _) <-localDebugCtx <$> get
    liftIO $ printF levelIndex index msg
    where
        levelIndex = fromEnum level

execDebugAction :: (Enum level) => level -> LogFeature -> (LocalTrans p) () -> (LocalTrans p) ()
execDebugAction level (LogFeature index) a = do
    (DebugCtx _ enabledF) <- localDebugCtx <$> get
    if enabledF levelIndex index then
        a
        else
            return ()
    where
        levelIndex = fromEnum level

ignoreLog _ _ _ = return ()
ignoreAction _ _ _ = return ()

instance CpuDebugM (LocalTrans p) Trace where
    cpuLog = printLog
    cpuDebugAction = execDebugAction

instance CpuDebugM (LocalTrans p) Debug where
    cpuLog = printLog
    cpuDebugAction = execDebugAction

instance CpuDebugM (LocalTrans p) Info where
    cpuLog = printLog
    cpuDebugAction = execDebugAction

instance CpuDebugM (LocalTrans p) Warning where
    cpuLog = printLog
    cpuDebugAction = execDebugAction

instance CpuDebugM (LocalTrans p) Error where
    cpuLog = printLog
    cpuDebugAction = execDebugAction

-------------------------------------------------------------------------------

localSchedulerAdd :: SchedId -> CpuCycles -> SchedHandler (LocalTrans p) -> (LocalTrans p) ()
localSchedulerAdd id cpuCycles handler = do
    scheduler <- localScheduler <$> get
    Log.cpuLogT Debug Log.PrismPc $ "Add " ++ (show id) ++ " " ++ (show scheduler)
    let time = convertToSchedTime cpuCycles
        scheduler_ = schedEventAdd scheduler id time handler
    modify $ \s -> s { localScheduler = scheduler_ }

localSchedulerAddDelta :: SchedId -> CpuCyclesDelta -> SchedHandler (LocalTrans p) -> (LocalTrans p) ()
localSchedulerAddDelta id delta handler = do
    scheduler <- localScheduler <$> get
    Log.cpuLogT Debug Log.PrismPc $ "Add delta " ++ (show id) ++ " " ++ (show scheduler)
    let time = convertToSchedTimeDelta delta
        scheduler_ = schedEventAddDelta scheduler id time handler
    modify $ \s -> s { localScheduler = scheduler_ }

localSchedulerRemove :: SchedId -> (LocalTrans p) ()
localSchedulerRemove id = do
    scheduler <- localScheduler <$> get
    Log.cpuLogT Debug Log.PrismPc $ "Remove " ++ (show id) ++ " " ++ (show scheduler)
    let scheduler_ = schedEventRemove scheduler id
    modify $ \s -> s { localScheduler = scheduler_ }

localSchedulerExpired :: CpuCycles -> (LocalTrans p) [(SchedHandlerOut (LocalTrans p))]
localSchedulerExpired cpuCycles = do
    scheduler <- localScheduler <$> get
    Log.cpuLogT Debug Log.PrismPc $ "Expired " ++ (show scheduler)
    let currentTime = convertToSchedTime cpuCycles
        (_, events, scheduler_) = expireSched scheduler currentTime
    modify $ \s -> s { localScheduler = scheduler_ }
    return events

localSchedulerReschedule :: CpuCycles -> (LocalTrans p) CpuCyclesDelta
localSchedulerReschedule cpuCycles = do
    scheduler <- localScheduler <$> get
    Log.cpuLogT Debug Log.PrismPc $ "Reschedule " ++ (show scheduler)
    let currentTime = convertToSchedTime cpuCycles
        (nextSchedTime, scheduler_) = reschedule scheduler currentTime
        nextSchedCycle = convertFromSchedTime <$> nextSchedTime
        cyclesP = maybe maxCpuCyclesDelta (calcCpuCyclesDelta cpuCycles) nextSchedCycle
    modify $ \s -> s { localScheduler = scheduler_ }
    return cyclesP

-------------------------------------------------------------------------------

instance Operand MMIOInternal8 (LocalTrans p) Uint8 where
    readOp (MMIOInternal8 (handlerIndex, offset)) = do
        peripherals <- get
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOMemType handlerIndex offset
            else do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                (peripheralMemRead8 handler) offset
    writeOp (MMIOInternal8 (handlerIndex, offset)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOMemType handlerIndex offset val
            else do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                (peripheralMemWrite8 handler) offset val

instance Operand MMIOInternal16 (LocalTrans p) Uint16 where
    readOp (MMIOInternal16 (handlerIndex, offset)) = do
        peripherals <- get
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOMemType handlerIndex offset
            else do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                (peripheralMemRead16 handler) offset
    writeOp (MMIOInternal16 (handlerIndex, offset)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOMemType handlerIndex offset val
            else do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                (peripheralMemWrite16 handler) offset val

-------------------------------------------------------------------------------

instance Operand PortInternal8 (LocalTrans p) Uint8 where
    readOp (PortInternal8 (handlerIndex, port)) = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port)
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                (peripheralPortRead8 handler) port
    writeOp (PortInternal8 (handlerIndex, port)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port) val
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                (peripheralPortWrite8 handler) port val

instance Operand PortInternal16 (LocalTrans p) Uint16 where
    readOp (PortInternal16 (handlerIndex, port)) = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port)
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                (peripheralPortRead16 handler) port
    writeOp (PortInternal16 (handlerIndex, port)) val = do
        peripherals <- get
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port) val
            else do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                (peripheralPortWrite16 handler) port val

-------------------------------------------------------------------------------
