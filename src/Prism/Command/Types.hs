{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Prism.Command.Types where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.STM

import Data.Word (Word32, Word16, Word8)
import Data.Set

import Foreign.Ptr
import Foreign.Marshal.Array (pokeArray)

import Prism.Cpu

-------------------------------------------------------------------------------

class PrismMsgQueue a b | a -> b where
    sendCpuMsgIO :: (MonadIO m) => a -> b -> m ()
    recvCpuMsgIO :: (MonadIO m) => a -> m b
    tryRecvCpuMsgIO :: (MonadIO m) => a -> m (Maybe b)

data PrismCpuCommand = PCmdInterruptUp PrismIRQ
                       | PCmdInterruptDown PrismIRQ
                       | PCmdPause
                       | PCmdStep
                       | PCmdCont
                       | PCmdBreak MemOffset
                       | PCmdBreakRemove MemOffset
                       | PCmdWriteMem Int [Word8]
                       | PCmdWriteReg8 Reg8 Uint8
                       | PCmdWriteReg16 Reg16 Uint16
                       | PCmdWriteRegSeg RegSeg Uint16
                       | PCmdReadCtx deriving (Show, Eq)

data PrismCpuResponse = PRspCtx Ctx
                        | PRspCont
                        | PRspStep deriving (Show)

newtype PrismCmdQueue = PrismCmdQueue (TQueue PrismCpuCommand)
newtype PrismRspQueue = PrismRspQueue (TQueue PrismCpuResponse)

data PrismComm = PrismComm {
        commCmdQueue :: PrismCmdQueue,
        commRspQueue :: PrismRspQueue,
        commBreakpoints :: Set MemOffset,
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

newPrismComm stopCPU = do
    queueCmd <- newTQueueIO :: IO (TQueue PrismCpuCommand)
    queueRsp <- newTQueueIO :: IO (TQueue PrismCpuResponse)
    return $ PrismComm (PrismCmdQueue queueCmd) (PrismRspQueue queueRsp) empty stopCPU

-------------------------------------------------------------------------------