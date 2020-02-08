module Peripherals.Local where

import Control.Monad.Trans

import Data.IORef
import qualified Data.Array as Array

import Prism
import PrismPeripheral

-------------------------------------------------------------------------------

data PeripheralsLocal p = PeripheralsLocal {
        localMaxPort :: IOHandlerIndex,
        localMaxMem :: IOHandlerIndex,
        localPeripheralPort :: PeripheralArray (PeripheralHandlerPort p),
        localPeripheralMem :: PeripheralArray (PeripheralHandlerMem p),
        localIOQueue :: IOQueue,
        localPeripherals :: IORef p
    }


instance IOMem (PeripheralsLocal p) where
    ioMemRead peripherals handlerIndex offset =
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOMemType handlerIndex offset
            else liftIO $ do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                devices <- readIORef $ localPeripherals peripherals
                (devices_, val) <- ioValMemRead devices handler offset
                writeIORef (localPeripherals peripherals) devices_
                return val
    ioMemWrite peripherals handlerIndex offset val =
        if handlerIndex <= (localMaxMem peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOMemType handlerIndex offset val
            else liftIO $ do
                let handler = (localPeripheralMem peripherals) Array.! handlerIndex
                devices <- readIORef $ localPeripherals peripherals
                devices_ <- ioValMemWrite devices handler offset val
                writeIORef (localPeripherals peripherals) devices_
                return ()


instance IOPort (PeripheralsLocal p) where
    ioPortRead peripherals handlerIndex port = 
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteRead (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port)
            else liftIO $ do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                devices <- readIORef $ localPeripherals peripherals
                (devices_, val) <- ioValPortRead devices handler port
                writeIORef (localPeripherals peripherals) devices_
                return val
    ioPortWrite peripherals handlerIndex port val =
        if handlerIndex <= (localMaxPort peripherals) then
            ioValRemoteWrite (localIOQueue peripherals) IOPortType handlerIndex (fromIntegral port) val
            else liftIO $ do
                let handler = (localPeripheralPort peripherals) Array.! handlerIndex
                devices <- readIORef $ localPeripherals peripherals
                devices_ <- ioValPortWrite devices handler port val
                writeIORef (localPeripherals peripherals) devices_
                return ()

-------------------------------------------------------------------------------
{-
createLocalPeripherals :: PeripheralLocal p -> IOQueue -> IO IOCtx 
createLocalPeripherals (PeripheralLocal maxPorts maxMem portRegion memRegion ports mem devices) queue = do
    ref <- newIORef devices
    return $ IOCtx (PeripheralsLocal maxPorts maxMem ports mem queue ref) memRegion portRegion
    -}
