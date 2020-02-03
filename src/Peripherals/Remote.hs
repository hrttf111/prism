module Peripherals.Remote where

import Control.Monad.Trans
import Control.Concurrent.STM.TQueue
import Control.Monad.STM (atomically)

import qualified Data.Array as Array

import Prism
import PrismPeripheral

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
