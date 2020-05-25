module Prism.Command.Exec where

import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.STM

import Data.Word (Word32, Word16, Word8)
import Data.Set

import Foreign.Ptr
import Foreign.Marshal.Array (pokeArray)

import Prism.Cpu
import Prism.Command.Types

-------------------------------------------------------------------------------
{-
sendAndWaitCpuMsg :: (PrismMsgQueue a1 b1, PrismMsgQueue a2 b2, MonadIO m) => a1 -> a2 -> b1 -> m b2
sendAndWaitCpuMsg queue1 queue2 msg1 = sendCpuMsgIO queue1 msg1 >> recvCpuMsgIO queue2

{-# SPECIALISE sendAndWaitCpuMsg :: (MonadIO m) => PrismCmdQueue -> PrismRspQueue -> PrismCpuCommand -> m PrismCpuResponse #-}

-------------------------------------------------------------------------------

processPrismCommand :: (MonadIO m) => PrismComm -> Ctx -> MemOffset -> m (Maybe (PrismComm, Ctx))
processPrismCommand comm ctx offset =
    if member offset (commBreakpoints comm) then do
            if commWaitResponse comm then processComm comm ctx
                else do
                    liftIO $ putStrLn $ "Break " ++ (show offset)
                    sendCpuMsgIO (commRspQueue comm) PRspCont
                    cpuProcessPause comm ctx
        else
            processComm comm ctx

processComm :: (MonadIO m) => PrismComm -> Ctx -> m (Maybe (PrismComm, Ctx))
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

cpuInterruptUp :: (MonadIO m) => PrismComm -> Ctx -> PrismIRQ -> m (Maybe (PrismComm, Ctx))
cpuInterruptUp comm ctx int = do
    (ioCtx_, intrOn) <- liftIO $ dispatchInterruptUp (ctxIO ctx) int
    let interrupts = (ctxInterrupts ctx)
        interrupts_ = if intrOn then interrupts {intIntrOn = True} else interrupts
    return $ Just (comm, ctx { ctxIO = ioCtx_, ctxInterrupts = interrupts_})

cpuInterruptDown :: (MonadIO m) => PrismComm -> Ctx -> PrismIRQ -> m (Maybe (PrismComm, Ctx))
cpuInterruptDown comm ctx int = do
    (ioCtx_, intrOn) <- liftIO $ dispatchInterruptDown (ctxIO ctx) int
    let interrupts = (ctxInterrupts ctx)
        interrupts_ = if not intrOn then interrupts {intIntrOn = False} else interrupts
    return $ Just (comm, ctx { ctxIO = ioCtx_, ctxInterrupts = interrupts_})

cpuProcessBreak :: (MonadIO m) => PrismComm -> Ctx -> Int -> m (Maybe (PrismComm, Ctx))
cpuProcessBreak comm ctx b = 
    return $ Just (comm {commBreakpoints = insert b (commBreakpoints comm)}, ctx)

cpuProcessBreakRemove :: (MonadIO m) => PrismComm -> Ctx -> Int -> m (Maybe (PrismComm, Ctx))
cpuProcessBreakRemove comm ctx b =
    return $ Just (comm {commBreakpoints = delete b (commBreakpoints comm)}, ctx)

cpuProcessPause :: (MonadIO m) => PrismComm -> Ctx -> m (Maybe (PrismComm, Ctx))
cpuProcessPause comm ctx = return $ Just (comm {commWaitResponse = True}, ctx)

cpuProcessStep :: (MonadIO m) => PrismComm -> Ctx -> m (Maybe (PrismComm, Ctx))
cpuProcessStep comm ctx =
    sendCpuMsgIO (commCmdQueue comm) PCmdPause
    >> sendCpuMsgIO (commRspQueue comm) PRspStep
    >> return Nothing

cpuProcessCont :: (MonadIO m) => PrismComm -> Ctx -> m (Maybe (PrismComm, Ctx))
cpuProcessCont comm ctx = do
    return $ Just (comm {commWaitResponse = False}, ctx)

cpuProcessMemWrite :: (MonadIO m) => PrismComm -> Ctx -> Int -> [Word8] -> m (Maybe (PrismComm, Ctx))
cpuProcessMemWrite comm ctx addr bytes = do
    liftIO $ pokeArray (ptrMem $ ctxMem ctx) bytes
    return $ Just (comm, ctx)
    where
        ptrMem (MemMain ptr) = plusPtr ptr addr

cpuProcessWriteReg8 :: (MonadIO m) => PrismComm -> Ctx -> Reg8 -> Word8 -> m (Maybe (PrismComm, Ctx))
cpuProcessWriteReg8 comm ctx reg val = do
    writeReg8 (ctxReg ctx) reg val
    return $ Just (comm, ctx)

cpuProcessWriteReg16 :: (MonadIO m) => PrismComm -> Ctx -> Reg16 -> Word16 -> m (Maybe (PrismComm, Ctx))
cpuProcessWriteReg16 comm ctx reg val = do
    writeReg16 (ctxReg ctx) reg val
    return $ Just (comm, ctx)

cpuProcessWriteRegSeg :: (MonadIO m) => PrismComm -> Ctx -> RegSeg -> Word16 -> m (Maybe (PrismComm, Ctx))
cpuProcessWriteRegSeg comm ctx reg val = do
    writeSeg (ctxReg ctx) reg val
    return $ Just (comm, ctx)

cpuProcessReadCtx :: (MonadIO m) => PrismComm -> Ctx -> m (Maybe (PrismComm, Ctx))
cpuProcessReadCtx comm ctx = do
    sendCpuMsgIO (commRspQueue comm) (PRspCtx ctx)
    return $ Just (comm, ctx)
-}
-------------------------------------------------------------------------------
