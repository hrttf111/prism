{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module PrismCommand where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.STM

import Data.Word (Word32, Word16, Word8)

import Prism 
import PrismCpu

-------------------------------------------------------------------------------

class PrismMsgQueue a b | a -> b where
    sendCpuMsgIO :: MonadIO m => a -> b -> m ()
    recvCpuMsgIO :: MonadIO m => a -> m b
    tryRecvCpuMsgIO :: MonadIO m => a -> m (Maybe b)

data PrismCpuCommand = PCmdInterrupt PrismInt
                       | PCmdPause
                       | PCmdStep
                       | PCmdCont
                       | PCmdReadCtx deriving (Show, Eq)

data PrismCpuResponse = PRspCtx Ctx
                        | PRspStep deriving (Show)

newtype PrismCmdQueue = PrismCmdQueue (TQueue PrismCpuCommand)
newtype PrismRspQueue = PrismRspQueue (TQueue PrismCpuResponse)

data PrismComm = PrismComm {
        commCmdQueue :: PrismCmdQueue,
        commRspQueue :: PrismRspQueue,
        commWaitResponse :: Bool
    }

instance PrismMsgQueue PrismCmdQueue PrismCpuCommand where
    sendCpuMsgIO (PrismCmdQueue queue) msg = liftIO . atomically $ writeTQueue queue msg
    recvCpuMsgIO (PrismCmdQueue queue) = liftIO . atomically $ readTQueue queue
    tryRecvCpuMsgIO (PrismCmdQueue queue) = liftIO . atomically $ tryReadTQueue queue

instance PrismMsgQueue PrismRspQueue PrismCpuResponse where
    sendCpuMsgIO (PrismRspQueue queue) msg = liftIO . atomically $ writeTQueue queue msg
    recvCpuMsgIO (PrismRspQueue queue) = liftIO . atomically $ readTQueue queue
    tryRecvCpuMsgIO (PrismRspQueue queue) = liftIO . atomically $ tryReadTQueue queue

-------------------------------------------------------------------------------

sendAndWaitCpuMsg :: (PrismMsgQueue a1 b1, PrismMsgQueue a2 b2, MonadIO m) => a1 -> a2 -> b1 -> m b2
sendAndWaitCpuMsg queue1 queue2 msg1 = sendCpuMsgIO queue1 msg1 >> recvCpuMsgIO queue2

{-# SPECIALISE sendAndWaitCpuMsg :: MonadIO m => PrismCmdQueue -> PrismRspQueue -> PrismCpuCommand -> m PrismCpuResponse #-}

-------------------------------------------------------------------------------

newPrismComm = do
    queueCmd <- newTQueueIO :: IO (TQueue PrismCpuCommand)
    queueRsp <- newTQueueIO :: IO (TQueue PrismCpuResponse)
    return $ PrismComm (PrismCmdQueue queueCmd) (PrismRspQueue queueRsp) False

processComm :: PrismComm -> Ctx -> PrismCtx IO (Maybe (PrismComm, Ctx))
processComm comm ctx = do
    msg <- if (commWaitResponse comm) then Just <$> recvCpuMsgIO (commCmdQueue comm) else tryRecvCpuMsgIO (commCmdQueue comm)
    case msg of
        Just m -> case m of
            PCmdInterrupt _ -> return Nothing
            PCmdPause -> cpuProcessPause comm ctx
            PCmdStep -> cpuProcessStep comm ctx
            PCmdCont -> cpuProcessCont comm ctx
            PCmdReadCtx -> cpuProcessReadCtx comm ctx
        Nothing -> return Nothing

cpuProcessPause :: PrismComm -> Ctx -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessPause comm ctx = return $ Just (comm {commWaitResponse = True}, ctx)

cpuProcessStep :: PrismComm -> Ctx -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessStep comm ctx =
    sendCpuMsgIO (commCmdQueue comm) PCmdPause
    >> return Nothing

cpuProcessCont :: PrismComm -> Ctx -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessCont comm ctx = return $ Just (comm {commWaitResponse = False}, ctx)

cpuProcessReadCtx :: PrismComm -> Ctx -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessReadCtx comm ctx = do
    sendCpuMsgIO (commRspQueue comm) (PRspCtx ctx)
    return $ Just (comm, ctx)

-------------------------------------------------------------------------------
