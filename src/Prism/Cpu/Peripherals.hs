{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module Prism.Cpu.Peripherals where

import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray

import Prism.Cpu.Types

-------------------------------------------------------------------------------

type IOHandlerIndex = Uint16
type IOPageIndex = Int -- div offset pageSize (or) shiftR n $ offset .&. mask
type IOPageOffset = Int -- mod offset pageSize (or) offset - IOPageIndex

data MemLocation = MemLocation {
        memLocationStart :: MemOffset,
        memLocationEnd :: MemOffset
    } deriving (Eq)

instance Show MemLocation where
    show (MemLocation start end) =
        "(" ++ (show start) ++ "," ++ (show end) ++ ")"

newtype IOPage = IOPage (UArray.UArray IOPageOffset IOHandlerIndex) deriving (Show, Eq)

newtype PortIORegion = PortIORegion (UArray.UArray Uint16 IOHandlerIndex) deriving (Show, Eq)

-- fill 3 arrays:
-- 1. L1 which is Int -> Int
--    is fixed size == memorySize/pageSize
--    unused indexes are occupied by emptyPage
-- 2. L2 which is Int -> IOPage
--    is variable size, depends on mapped pages count
--    indexes for pages are generated in run-time
-- 3. IOPage which is Uint16 -> Uint16
--    is fixed size
--    unused indexes are occupied by emptyHandler

data MemIORegion = MemIORegion {
        ioPageSize :: Int,
        ioRegionL1 :: UArray.UArray Int IOPageIndex,
        ioRegionL2 :: Array.Array IOPageIndex IOPage
    } deriving (Show)

-------------------------------------------------------------------------------

emptyPage = 0
emptyHandler = 0

findMemIndex :: MemIORegion -> MemOffset -> IOHandlerIndex
findMemIndex (MemIORegion pageSize l1 l2) memOffset = 
    if pageIndex /= emptyPage then
        let IOPage pageArray = l2 Array.! pageIndex
            in
        pageArray UArray.! (mod memOffset pageSize)
        else emptyHandler
    where
        pageIndex = l1 UArray.! (div memOffset pageSize)

findPortIndex :: PortIORegion -> Uint16 -> IOHandlerIndex
findPortIndex (PortIORegion arr) port =
    arr UArray.! port

-------------------------------------------------------------------------------

newtype PortInternal8 = PortInternal8 (IOHandlerIndex, Uint16) deriving (Eq, Show)
newtype PortInternal16 = PortInternal16 (IOHandlerIndex, Uint16) deriving (Eq, Show)

newtype MMIOInternal8 = MMIOInternal8 (IOHandlerIndex, MemOffset) deriving (Eq, Show)
newtype MMIOInternal16 = MMIOInternal16 (IOHandlerIndex, MemOffset) deriving (Eq, Show)

-------------------------------------------------------------------------------

class ( Monad m
      , Operand PortInternal8 m Uint8
      , Operand PortInternal16 m Uint16
      , Operand MMIOInternal8 m Uint8
      , Operand MMIOInternal16 m Uint16
      , InterruptDispatcher m
      ) => PeripheralsMonad m where
      runPeripherals :: m ()

class ( Monad mc
      , PeripheralsMonad mp
      ) => RunPeripheralsM a mp mc | a -> mp mc where
    runPeripheralsM :: a -> mp b -> mc b

class (Monad m) => RunPeripheralsDirect a m | a -> m where
    runPeripheralsDirect :: a -> DirectCommand -> m ()

-------------------------------------------------------------------------------
