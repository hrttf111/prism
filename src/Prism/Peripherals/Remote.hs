{-# LANGUAGE TypeSynonymInstances #-}

module Prism.Peripherals.Remote where

import Control.Monad.STM (atomically)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict
import Control.Concurrent.STM.TQueue

import qualified Data.Array as Array

import Prism.Cpu
import Prism.Peripherals.Types
import Prism.Peripherals.Queue

-------------------------------------------------------------------------------

execPeripheralsOnce :: IOQueue -> Peripheral p -> IO ()
execPeripheralsOnce queue@(IOQueue req rsp) peripheral = do
    msg <- atomically $ readTQueue req
    putStrLn $ "Got message " ++ (show msg)
    let devices = peripheralDevices peripheral
    peripheralNew <- (case msg of
        IOCmdRead8 IOMemType handlerIndex memOffset -> do
            let handler = (peripheralMem peripheral) Array.! handlerIndex
            (per, val) <- ioValMemRead devices handler memOffset
            atomically $ writeTQueue rsp $ IOCmdData8 val
            return $ peripheral { peripheralDevices = per }
        IOCmdRead16 IOMemType handlerIndex memOffset -> do
            let handler = (peripheralMem peripheral) Array.! handlerIndex
            (per, val) <- ioValMemRead devices handler memOffset
            atomically $ writeTQueue rsp $ IOCmdData16 val
            return $ peripheral { peripheralDevices = per }
        IOCmdWrite8 IOMemType handlerIndex memOffset val -> do
            let handler = (peripheralMem peripheral) Array.! handlerIndex
            per <- ioValMemWrite devices handler memOffset val
            return $ peripheral { peripheralDevices = per }
        IOCmdWrite16 IOMemType handlerIndex memOffset val -> do
            let handler = (peripheralMem peripheral) Array.! handlerIndex
            per <- ioValMemWrite devices handler memOffset val
            return $ peripheral { peripheralDevices = per }
        IOCmdRead8 IOPortType handlerIndex memOffset -> do
            let handler = (peripheralPort peripheral) Array.! handlerIndex
            (per, val) <- ioValPortRead devices handler $ fromIntegral memOffset
            atomically $ writeTQueue rsp $ IOCmdData8 val
            return $ peripheral { peripheralDevices = per }
        IOCmdRead16 IOPortType handlerIndex memOffset -> do
            let handler = (peripheralPort peripheral) Array.! handlerIndex
            (per, val) <- ioValPortRead devices handler $ fromIntegral memOffset
            atomically $ writeTQueue rsp $ IOCmdData16 val
            return $ peripheral { peripheralDevices = per }
        IOCmdWrite8 IOPortType handlerIndex memOffset val -> do
            let handler = (peripheralPort peripheral) Array.! handlerIndex
            per <- ioValPortWrite devices handler (fromIntegral memOffset) val
            return $ peripheral { peripheralDevices = per }
        IOCmdWrite16 IOPortType handlerIndex memOffset val -> do
            let handler = (peripheralPort peripheral) Array.! handlerIndex
            per <- ioValPortWrite devices handler (fromIntegral memOffset) val
            return $ peripheral { peripheralDevices = per }
        _ -> return peripheral
        )
    putStrLn "Thread end"
    return ()

-------------------------------------------------------------------------------

instance IOValMem Uint8 where
    ioValMemRead devices handler offset =
        liftIO $ (peripheralMemRead8 handler) devices offset
    ioValMemWrite devices handler offset val =
        liftIO $ (peripheralMemWrite8 handler) devices offset val

instance IOValPort Uint8 where
    ioValPortRead devices handler port =
        liftIO $ (peripheralPortRead8 handler) devices port
    ioValPortWrite devices handler port val =
        liftIO $ (peripheralPortWrite8 handler) devices port val

instance IOValMem Uint16 where
    ioValMemRead devices handler offset =
        liftIO $ (peripheralMemRead16 handler) devices offset
    ioValMemWrite devices handler offset val =
        liftIO $ (peripheralMemWrite16 handler) devices offset val

instance IOValPort Uint16 where
    ioValPortRead devices handler port =
        liftIO $ (peripheralPortRead16 handler) devices port
    ioValPortWrite devices handler port val =
        liftIO $ (peripheralPortWrite16 handler) devices port val

-------------------------------------------------------------------------------
