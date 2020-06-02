module Prism.Peripherals.Types where

import Control.Monad.Trans (MonadIO)
import Control.Exception (Exception)

import qualified Data.Array as Array

import Prism.Cpu

-------------------------------------------------------------------------------

data IOCtxException = IOCtxException deriving Show

instance Exception IOCtxException

------------------------------------------------------------------------------

data PeripheralHandlerMem m = PeripheralHandlerMem {
        peripheralMemWrite8 :: MemOffset -> Uint8 -> m (),
        peripheralMemWrite16 :: MemOffset -> Uint16 -> m (),
        peripheralMemRead8 :: MemOffset -> m Uint8,
        peripheralMemRead16 :: MemOffset -> m Uint16
    }

data PeripheralHandlerPort m = PeripheralHandlerPort {
        peripheralPortWrite8 :: Uint16 -> Uint8 -> m (),
        peripheralPortWrite16 :: Uint16 -> Uint16 -> m (),
        peripheralPortRead8 :: Uint16 -> m Uint8,
        peripheralPortRead16 :: Uint16 -> m Uint16
    }
    
data PeripheralPort m = PeripheralPort {
        peripheralPortLoc :: Uint16,
        peripheralPortHandlers :: PeripheralHandlerPort m
    }

data PeripheralMem m = PeripheralMem {
        peripheralMemLoc :: MemLocation,
        peripheralMemHandlers :: PeripheralHandlerMem m
    }

instance Show (PeripheralMem p) where
    show (PeripheralMem loc _) = show loc

instance Eq (PeripheralMem p) where
    item1 == item2 = (peripheralMemLoc item1) == (peripheralMemLoc item2)

-------------------------------------------------------------------------------

type PeripheralArray p = Array.Array IOHandlerIndex p

data Peripheral m p = Peripheral {
        peripheralPortRegion :: PortIORegion,
        peripheralMemRegion :: MemIORegion,
        peripheralPort :: PeripheralArray (PeripheralHandlerPort m),
        peripheralMem :: PeripheralArray (PeripheralHandlerMem m),
        peripheralDevices :: p
    }

data PeripheralLocal m p = PeripheralLocal {
        peripheralLocalMaxPortL :: IOHandlerIndex,
        peripheralLocalMaxMemL :: IOHandlerIndex,
        peripheralPortRegionL :: PortIORegion,
        peripheralMemRegionL :: MemIORegion,
        peripheralPortL :: PeripheralArray (PeripheralHandlerPort m),
        peripheralMemL :: PeripheralArray (PeripheralHandlerMem m),
        peripheralDevicesL :: p
    }

-------------------------------------------------------------------------------

emptyReadH :: (Monad m, OperandVal b) => a -> m b
emptyReadH _ = return 0

emptyWriteH :: (Monad m) => a -> b -> m ()
emptyWriteH _ _ = return ()

emptyMemHandler :: (Monad m) => PeripheralHandlerMem m
emptyMemHandler = 
    PeripheralHandlerMem emptyWriteH emptyWriteH emptyReadH emptyReadH

emptyPortHandler :: (Monad m) => PeripheralHandlerPort m
emptyPortHandler =
    PeripheralHandlerPort emptyWriteH emptyWriteH emptyReadH emptyReadH

-------------------------------------------------------------------------------
