module Prism.Peripherals.Types where

import Control.Monad.Trans (MonadIO)
import Control.Exception (Exception)

import qualified Data.Array as Array

import Prism.Cpu

-------------------------------------------------------------------------------

data IOCtxException = IOCtxException deriving Show

instance Exception IOCtxException

------------------------------------------------------------------------------

data PeripheralHandlerMem p = PeripheralHandlerMem {
        peripheralMemWrite8 :: p -> MemOffset -> Uint8 -> IO p,
        peripheralMemWrite16 :: p -> MemOffset -> Uint16 -> IO p,
        peripheralMemRead8 :: p -> MemOffset -> IO (p, Uint8),
        peripheralMemRead16 :: p -> MemOffset -> IO (p, Uint16)
    }

data PeripheralHandlerPort p = PeripheralHandlerPort {
        peripheralPortWrite8 :: p -> Uint16 -> Uint8 -> IO p,
        peripheralPortWrite16 :: p -> Uint16 -> Uint16 -> IO p,
        peripheralPortRead8 :: p -> Uint16 -> IO (p, Uint8),
        peripheralPortRead16 :: p -> Uint16 -> IO (p, Uint16)
    }
    
data PeripheralPort p = PeripheralPort {
        peripheralPortLoc :: Uint16,
        peripheralPortHandlers :: PeripheralHandlerPort p
    }

data PeripheralMem p = PeripheralMem {
        peripheralMemLoc :: MemLocation,
        peripheralMemHandlers :: PeripheralHandlerMem p
    }

instance Show (PeripheralMem p) where
    show (PeripheralMem loc _) = show loc

instance Eq (PeripheralMem p) where
    item1 == item2 = (peripheralMemLoc item1) == (peripheralMemLoc item2)

-------------------------------------------------------------------------------

type PeripheralArray p = Array.Array IOHandlerIndex p

data Peripheral p = Peripheral {
        peripheralPortRegion :: PortIORegion,
        peripheralMemRegion :: MemIORegion,
        peripheralPort :: PeripheralArray (PeripheralHandlerPort p),
        peripheralMem :: PeripheralArray (PeripheralHandlerMem p),
        peripheralDevices :: p
    }

data PeripheralLocal p = PeripheralLocal {
        peripheralLocalMaxPortL :: IOHandlerIndex,
        peripheralLocalMaxMemL :: IOHandlerIndex,
        peripheralPortRegionL :: PortIORegion,
        peripheralMemRegionL :: MemIORegion,
        peripheralPortL :: PeripheralArray (PeripheralHandlerPort p),
        peripheralMemL :: PeripheralArray (PeripheralHandlerMem p),
        peripheralDevicesL :: p
    }

-------------------------------------------------------------------------------

class (OperandVal a) => IOValMem a where
    ioValMemRead :: (MonadIO m) => p -> PeripheralHandlerMem p -> MemOffset -> m (p, a)
    ioValMemWrite :: (MonadIO m) => p -> PeripheralHandlerMem p -> MemOffset -> a -> m p

class (OperandVal a) => IOValPort a where
    ioValPortRead :: (MonadIO m) => p -> PeripheralHandlerPort p -> Uint16 -> m (p, a)
    ioValPortWrite :: (MonadIO m) => p -> PeripheralHandlerPort p -> Uint16 -> a -> m p

-------------------------------------------------------------------------------

emptyReadH :: (OperandVal b) => p -> a -> IO (p, b)
emptyReadH p _ = return (p, 0)

emptyWriteH :: p -> a -> b -> IO p
emptyWriteH p _ _ = return p

emptyMemHandler :: PeripheralHandlerMem p
emptyMemHandler = 
    PeripheralHandlerMem emptyWriteH emptyWriteH emptyReadH emptyReadH

emptyPortHandler :: PeripheralHandlerPort p
emptyPortHandler =
    PeripheralHandlerPort emptyWriteH emptyWriteH emptyReadH emptyReadH

-------------------------------------------------------------------------------
