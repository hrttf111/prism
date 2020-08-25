{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Prism.Command.Types where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.STM

import qualified Data.ByteString as B
import Data.Word (Word32, Word16, Word8)
import Data.Set

import Foreign.Ptr
import Foreign.Marshal.Array (pokeArray)

import Prism.Cpu

-------------------------------------------------------------------------------

data RegState = RegState {
        rsAX :: Uint16,
        rsCX :: Uint16,
        rsDX :: Uint16,
        rsBX :: Uint16,
        rsSP :: Uint16,
        rsBP :: Uint16,
        rsSI :: Uint16,
        rsDI :: Uint16,
        rsFlags :: Flags,
        rsEFlags :: EFlags,
        rsCS :: Uint16,
        rsSS :: Uint16,
        rsDS :: Uint16,
        rsES :: Uint16,
        rsIP :: Uint16
    } deriving (Show)

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
                       | PCmdReadRegs
                       | PCmdReadMem MemOffset Int
                       | PCmdWriteMem Int [Word8]
                       | PCmdWriteReg8 Reg8 Uint8
                       | PCmdWriteReg16 Reg16 Uint16
                       | PCmdWriteRegSeg RegSeg Uint16
                       deriving (Show, Eq)

data PrismCpuResponse = PRspRegs RegState
                        | PRspMem B.ByteString
                        | PRspCont
                        | PRspStep deriving (Show)

newtype PrismCmdQueue = PrismCmdQueue (TQueue PrismCpuCommand)
newtype PrismRspQueue = PrismRspQueue (TQueue PrismCpuResponse)

data PrismComm = PrismComm {
        commCmdQueue :: PrismCmdQueue,
        commRspQueue :: PrismRspQueue,
        commBreakpoints :: Set MemOffset,
        commBreakpointsEnabled :: Bool,
        commWaitResponse :: Bool,
        commCycles :: CpuCycles
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
    return $ PrismComm (PrismCmdQueue queueCmd) (PrismRspQueue queueRsp) empty stopCPU False 0

-------------------------------------------------------------------------------
