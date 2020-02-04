{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module PrismCommand where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.STM

import Data.Word (Word32, Word16, Word8)
import Data.Set

import Foreign.Ptr
import Foreign.Marshal.Array (pokeArray)

import Prism 
import PrismCpu

-------------------------------------------------------------------------------

class PrismMsgQueue a b | a -> b where
    sendCpuMsgIO :: MonadIO m => a -> b -> m ()
    recvCpuMsgIO :: MonadIO m => a -> m b
    tryRecvCpuMsgIO :: MonadIO m => a -> m (Maybe b)

data PrismCpuCommand = PCmdInterruptUp PrismInt
                       | PCmdInterruptDown PrismInt 
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

-------------------------------------------------------------------------------

sendAndWaitCpuMsg :: (PrismMsgQueue a1 b1, PrismMsgQueue a2 b2, MonadIO m) => a1 -> a2 -> b1 -> m b2
sendAndWaitCpuMsg queue1 queue2 msg1 = sendCpuMsgIO queue1 msg1 >> recvCpuMsgIO queue2

{-# SPECIALISE sendAndWaitCpuMsg :: MonadIO m => PrismCmdQueue -> PrismRspQueue -> PrismCpuCommand -> m PrismCpuResponse #-}

-------------------------------------------------------------------------------

newPrismComm stopCPU = do
    queueCmd <- newTQueueIO :: IO (TQueue PrismCpuCommand)
    queueRsp <- newTQueueIO :: IO (TQueue PrismCpuResponse)
    return $ PrismComm (PrismCmdQueue queueCmd) (PrismRspQueue queueRsp) empty stopCPU

processPrismCommand :: PrismComm -> Ctx -> MemOffset -> PrismCtx IO (Maybe (PrismComm, Ctx))
processPrismCommand comm ctx offset =
    if member offset (commBreakpoints comm) then do
            if commWaitResponse comm then processComm comm ctx
                else do
                    liftIO $ putStrLn $ "Break " ++ (show offset)
                    sendCpuMsgIO (commRspQueue comm) PRspCont
                    cpuProcessPause comm ctx
        else
            processComm comm ctx

processComm :: PrismComm -> Ctx -> PrismCtx IO (Maybe (PrismComm, Ctx))
processComm comm ctx = do
    msg <- tryRecvCpuMsgIO (commCmdQueue comm)
    case msg of
        Just m -> case m of
            PCmdBreak addr -> cpuProcessBreak comm ctx addr
            PCmdBreakRemove addr -> cpuProcessBreakRemove comm ctx addr
            PCmdInterruptUp i -> cpuInterruptUp comm ctx i
            PCmdInterruptDown i -> cpuInterruptDown comm ctx i 
            PCmdPause -> cpuProcessPause comm ctx
            PCmdStep -> cpuProcessStep comm ctx
            PCmdCont -> cpuProcessCont comm ctx
            PCmdWriteMem addr bytes -> cpuProcessMemWrite comm ctx addr bytes
            PCmdWriteReg8 reg val -> cpuProcessWriteReg8 comm ctx reg val
            PCmdWriteReg16 reg val -> cpuProcessWriteReg16 comm ctx reg val
            PCmdWriteRegSeg reg val -> cpuProcessWriteRegSeg comm ctx reg val
            PCmdReadCtx -> cpuProcessReadCtx comm ctx
        Nothing ->
            if commWaitResponse comm then processComm comm ctx
                else return Nothing

cpuInterruptUp :: PrismComm -> Ctx -> PrismInt -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuInterruptUp comm ctx int = do
    (ioCtx_, intrOn) <- liftIO $ dispatchInterruptUp (ctxIO ctx) int
    let interrupts = (ctxInterrupts ctx)
        interrupts_ = if intrOn then interrupts {intIntrOn = True} else interrupts
    return $ Just (comm, ctx { ctxIO = ioCtx_, ctxInterrupts = interrupts_})

cpuInterruptDown :: PrismComm -> Ctx -> PrismInt -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuInterruptDown comm ctx int = do
    (ioCtx_, intrOn) <- liftIO $ dispatchInterruptDown (ctxIO ctx) int
    let interrupts = (ctxInterrupts ctx)
        interrupts_ = if not intrOn then interrupts {intIntrOn = False} else interrupts
    return $ Just (comm, ctx { ctxIO = ioCtx_, ctxInterrupts = interrupts_})

cpuProcessBreak :: PrismComm -> Ctx -> Int -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessBreak comm ctx b = 
    return $ Just (comm {commBreakpoints = insert b (commBreakpoints comm)}, ctx)

cpuProcessBreakRemove :: PrismComm -> Ctx -> Int -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessBreakRemove comm ctx b =
    return $ Just (comm {commBreakpoints = delete b (commBreakpoints comm)}, ctx)

cpuProcessPause :: PrismComm -> Ctx -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessPause comm ctx = return $ Just (comm {commWaitResponse = True}, ctx)

cpuProcessStep :: PrismComm -> Ctx -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessStep comm ctx =
    sendCpuMsgIO (commCmdQueue comm) PCmdPause
    >> sendCpuMsgIO (commRspQueue comm) PRspStep
    >> return Nothing

cpuProcessCont :: PrismComm -> Ctx -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessCont comm ctx = do
    return $ Just (comm {commWaitResponse = False}, ctx)

cpuProcessMemWrite :: PrismComm -> Ctx -> Int -> [Word8] -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessMemWrite comm ctx addr bytes = do
    liftIO $ pokeArray (ptrMem $ ctxMem ctx) bytes
    return $ Just (comm, ctx)
    where
        ptrMem (MemMain ptr) = plusPtr ptr addr

cpuProcessWriteReg8 :: PrismComm -> Ctx -> Reg8 -> Word8 -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessWriteReg8 comm ctx reg val = do
    writeReg8 (ctxReg ctx) reg val
    return $ Just (comm, ctx)

cpuProcessWriteReg16 :: PrismComm -> Ctx -> Reg16 -> Word16 -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessWriteReg16 comm ctx reg val = do
    writeReg16 (ctxReg ctx) reg val
    return $ Just (comm, ctx)

cpuProcessWriteRegSeg :: PrismComm -> Ctx -> RegSeg -> Word16 -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessWriteRegSeg comm ctx reg val = do
    writeSeg (ctxReg ctx) reg val
    return $ Just (comm, ctx)

cpuProcessReadCtx :: PrismComm -> Ctx -> PrismCtx IO (Maybe (PrismComm, Ctx))
cpuProcessReadCtx comm ctx = do
    sendCpuMsgIO (commRspQueue comm) (PRspCtx ctx)
    return $ Just (comm, ctx)

-------------------------------------------------------------------------------
