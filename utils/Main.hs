{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.State
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.Trans (MonadIO, liftIO)

import Data.List (null, repeat, cycle)
import qualified Data.Text as T
import Data.Word (Word8)
import qualified Data.ByteString as BS
import Data.Array (Array, array, listArray, (!), bounds, elems, (//), ixmap)

import System.Environment (getArgs)
import System.IO(FilePath, stdout, Handle, withFile, IOMode(..))

import GDB.GDB
import GDB.Server
import GDB.Protocol
import GDB.Logger

data ClientState = ClientState {
        clientReqQ :: CmdQueue,
        clientResQ :: CmdQueue,
        clientCode :: Array Int Word8,
        clientRegs :: Array Int Int
    }

runClientFile :: FilePath -> CmdQueue -> CmdQueue -> IO ()
runClientFile filePath queueReq queueRes = do 
    bs <- BS.readFile filePath
    let regs = listArray (0, 15) $ repeat 0x01
        code = listArray (0, 1024*1024) $ BS.unpack bs
    cpuThread $ ClientState queueReq queueRes code regs

runClientGen :: CmdQueue -> CmdQueue -> IO ()
runClientGen queueReq queueRes = do 
    let regs = listArray (0, 15) $ repeat 0x01
        code = listArray (0, 1024*1024) (cycle [1..99])
    cpuThread $ ClientState queueReq queueRes code regs

cpuThread :: ClientState -> IO ()
cpuThread state = do 
    msg <- atomically $ readTQueue queueReq
    case msg of
        CpuReq CpuReadRegsReq -> do
            putStrLn "Read REGS"
            atomically $ writeTQueue queueRes $ CpuResp (CpuReadRegRes $ map fromIntegral $ elems regs)
            cpuThread state
        CpuReq (CpuWriteReg reg val) -> do
            putStrLn $ "Write reg " ++ (show reg) ++ " = " ++ (show val)
            cpuThread $ state { clientRegs = (regs // [(reg, val)]) }
        CpuReq (CpuWriteMem addr val) -> do
            putStrLn $ "Write mem " ++ (show addr) ++ " = " ++ (show val)
            cpuThread state
        CpuReq (CpuReadMem addr size) -> do
            putStrLn $ "Read mem " ++ (show addr) ++ " " ++ (show size)
            let slice = ixmap (addr, addr+size) id (clientCode state)
            putStrLn $ show slice
            atomically $ writeTQueue queueRes $ CpuResp (CpuReadMemRes $ elems slice)
            cpuThread state
        CpuReq CpuStepReq -> do
            putStrLn "Step"
            atomically $ writeTQueue queueRes $ CpuResp CpuStepRes
            cpuThread $ state { clientRegs = (regs // [(8, (regs!8) + 1)]) }
        CpuReq CpuContReq -> do
            putStrLn "Cont"
            atomically $ writeTQueue queueRes $ CpuResp CpuContRes
            cpuThread $ state { clientRegs = (regs // [(8, (regs!8) + 1)]) }
        CpuReq (CpuBreakReq addr)-> do
            putStrLn "Break"
            atomically $ writeTQueue queueRes $ CpuResp CpuTrapRes
            cpuThread $ state { clientRegs = (regs // [(8, addr)]) }
        _ -> do
            putStrLn "Uknown CPU command"
            cpuThread state
    where
        queueReq = clientReqQ state
        queueRes = clientResQ state
        regs = clientRegs state

main :: IO ()
main = do
    {-args <- getArgs
    queue1 <- newTQueueIO :: IO CmdQueue
    queue2 <- newTQueueIO :: IO CmdQueue
    if null args then
        forkIO $ runClientGen queue1 queue2 
    else
        forkIO $ runClientFile (head args) queue1 queue2 
    runStateT (runMyLoggingT (runGDB $ runServer "127.0.0.1" 20001)) $ emptyO queue1 queue2
    -}
    putStrLn "End"
