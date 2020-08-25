module Prism.Command.Exec where

import Control.Monad.State.Strict
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.STM

import Data.ByteString.Internal (createUptoN)
import Data.Word (Word32, Word16, Word8)
import Data.Set

import Foreign.Ptr
import Foreign.Marshal.Array (pokeArray, copyArray)

import Prism.Cpu
import Prism.Command.Types

-------------------------------------------------------------------------------

sendAndWaitCpuMsg :: (PrismMsgQueue a1 b1, PrismMsgQueue a2 b2, MonadIO m) => a1 -> a2 -> b1 -> m b2
sendAndWaitCpuMsg queue1 queue2 msg1 = sendCpuMsgIO queue1 msg1 >> recvCpuMsgIO queue2

{-# SPECIALISE sendAndWaitCpuMsg :: (MonadIO m) => PrismCmdQueue -> PrismRspQueue -> PrismCpuCommand -> m PrismCpuResponse #-}

-------------------------------------------------------------------------------

processPrismCommand :: PrismComm -> MemOffset -> PrismM PrismComm
processPrismCommand comm offset = do
    cycles <- ctxCycles <$> get
    if needUpdateComm comm cycles then
        updateComm comm offset
        else
            return comm

needUpdateComm :: PrismComm -> CpuCycles -> Bool
needUpdateComm comm cycles =
    (commBreakpointsEnabled comm) || (cycles > commCycles comm)

updateComm :: PrismComm -> MemOffset -> PrismM PrismComm
updateComm comm offset = do
    if member offset (commBreakpoints comm) then do
            if commWaitResponse comm then processComm comm
                else do
                    liftIO $ putStrLn $ "Break " ++ (show offset)
                    sendCpuMsgIO (commRspQueue comm) PRspCont
                    cpuProcessPause comm
        else
            processComm comm

processComm :: PrismComm -> PrismM PrismComm
processComm comm = do
    msg <- tryRecvCpuMsgIO (commCmdQueue comm)
    case msg of
        Just m -> case m of
            PCmdBreak addr -> cpuProcessBreak comm addr
            PCmdBreakRemove addr -> cpuProcessBreakRemove comm addr
            PCmdInterruptUp i -> cpuInterruptUp comm i
            PCmdInterruptDown i -> cpuInterruptDown comm i 
            PCmdPause -> cpuProcessPause comm
            PCmdStep -> cpuProcessStep comm
            PCmdCont -> cpuProcessCont comm
            PCmdWriteMem addr bytes -> cpuProcessMemWrite comm addr bytes
            PCmdWriteReg8 reg val -> cpuProcessWriteReg8 comm reg val
            PCmdWriteReg16 reg val -> cpuProcessWriteReg16 comm reg val
            PCmdWriteRegSeg reg val -> cpuProcessWriteRegSeg comm reg val
            PCmdReadMem offset size -> cpuProcessReadMem comm offset size
            PCmdReadRegs -> cpuProcessReadRegs comm
        Nothing ->
            if commWaitResponse comm then processComm comm
                else do
                    c <- ctxCycles <$> get
                    return $ comm { commCycles = (c + CpuCycles 10) }

cpuInterruptUp :: PrismComm -> PrismIRQ -> PrismM PrismComm
cpuInterruptUp comm irq =
    dispatchIrqUp irq >> return comm

cpuInterruptDown :: PrismComm -> PrismIRQ -> PrismM PrismComm
cpuInterruptDown comm irq =
    dispatchIrqDown irq >> return comm

cpuProcessBreak :: PrismComm -> Int -> PrismM PrismComm
cpuProcessBreak comm b = 
    return $ comm {commBreakpoints = insert b (commBreakpoints comm), commBreakpointsEnabled = True}

cpuProcessBreakRemove :: PrismComm -> Int -> PrismM PrismComm
cpuProcessBreakRemove comm b =
    return $ comm {commBreakpoints = newSet, commBreakpointsEnabled = Data.Set.null newSet}
    where
        newSet = delete b (commBreakpoints comm)

cpuProcessPause :: PrismComm -> PrismM PrismComm
cpuProcessPause comm = return $ comm {commWaitResponse = True}

cpuProcessStep :: PrismComm -> PrismM PrismComm
cpuProcessStep comm =
    sendCpuMsgIO (commCmdQueue comm) PCmdPause
    >> sendCpuMsgIO (commRspQueue comm) PRspStep
    >> return comm

cpuProcessCont :: PrismComm -> PrismM PrismComm
cpuProcessCont comm = do
    return $ comm {commWaitResponse = False}

cpuProcessMemWrite :: PrismComm -> Int -> [Word8] -> PrismM PrismComm
cpuProcessMemWrite comm addr bytes = do
    ctx <- get
    liftIO $ pokeArray (ptrMem $ ctxMem ctx) bytes
    return comm
    where
        ptrMem (MemMain ptr) = plusPtr ptr addr

cpuProcessWriteReg8 :: PrismComm -> Reg8 -> Word8 -> PrismM PrismComm
cpuProcessWriteReg8 comm reg val = do
    writeOp reg val
    return comm

cpuProcessWriteReg16 :: PrismComm -> Reg16 -> Word16 -> PrismM PrismComm
cpuProcessWriteReg16 comm reg val = do
    writeOp reg val
    return comm

cpuProcessWriteRegSeg :: PrismComm -> RegSeg -> Word16 -> PrismM PrismComm
cpuProcessWriteRegSeg comm reg val = do
    writeOp reg val
    return comm

cpuProcessReadMem :: PrismComm -> MemOffset -> Int -> PrismM PrismComm
cpuProcessReadMem comm offset size = do
    ctx <- get
    b <- liftIO $ createUptoN size $ readM (ctxMem ctx)
    sendCpuMsgIO (commRspQueue comm) (PRspMem b)
    return comm
    where
        readM (MemMain ptr) ptrB = do
            copyArray ptr ptrB size
            return size

cpuProcessReadRegs :: PrismComm -> PrismM PrismComm
cpuProcessReadRegs comm = do
    rs <- (RegState <$> readOp ax
                 <*> readOp cx
                 <*> readOp dx
                 <*> readOp bx
                 <*> readOp sp
                 <*> readOp bp
                 <*> readOp si
                 <*> readOp di
                 <*> getFlags
                 <*> getFlags
                 <*> readOp cs
                 <*> readOp ss
                 <*> readOp ds
                 <*> readOp es
                 <*> readOp ip)
    sendCpuMsgIO (commRspQueue comm) (PRspRegs rs)
    return comm

-------------------------------------------------------------------------------
