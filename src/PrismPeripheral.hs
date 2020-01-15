{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PrismPeripheral where

import Prism
import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray
import Data.List (partition, sortOn, zip, takeWhile)

-------------------------------------------------------------------------------

type PeripheralWrite8 = Peripheral -> MemOffset -> Uint8 -> IO Peripheral

data PeripheralHandlerMem = PeripheralHandlerMem {
        peripheralMemWrite8 :: Peripheral -> MemOffset -> Uint8 -> IO Peripheral,
        peripheralMemWrite16 :: Peripheral -> MemOffset -> Uint16 -> IO Peripheral,
        peripheralMemRead8 :: Peripheral -> MemOffset -> IO (Peripheral, Uint8),
        peripheralMemRead16 :: Peripheral -> MemOffset -> IO (Peripheral, Uint16)
    }

emptyReadH :: OperandVal b => Peripheral -> a -> IO (Peripheral, b)
emptyReadH p _ = return (p, 0)

emptyWriteH :: Peripheral -> a -> b -> IO Peripheral
emptyWriteH p _ _ = return p

emptyMemHandler :: PeripheralHandlerMem
emptyMemHandler = 
    PeripheralHandlerMem emptyWriteH emptyWriteH emptyReadH emptyReadH

data PeripheralHandlerPort = PeripheralHandlerPort {
        peripheralPortWrite8 :: Peripheral -> Uint16 -> Uint8 -> IO Peripheral,
        peripheralPortWrite16 :: Peripheral -> Uint16 -> Uint16 -> IO Peripheral,
        peripheralPortRead8 :: Peripheral -> Uint16 -> IO (Peripheral, Uint8),
        peripheralPortRead16 :: Peripheral -> Uint16 -> IO (Peripheral, Uint16)
    }

data PeripheralPort = PeripheralPort {
        peripheralPortLoc :: Uint16,
        peripheralPortHandlers :: PeripheralHandlerPort
    }
data PeripheralMem = PeripheralMem {
        peripheralMemLoc :: (MemOffset, MemOffset),
        peripheralMemHandlers :: PeripheralHandlerMem
    }

instance Show PeripheralMem where
    show (PeripheralMem (start, end) _) = 
        "(" ++ (show start) ++ "," ++ (show end) ++ ")"

instance Eq PeripheralMem where
    item1 == item2 = (peripheralMemLoc item1) == (peripheralMemLoc item2)

-------------------------------------------------------------------------------

data PeripheralDevices = PeripheralDevices {
    }

data Peripheral = Peripheral {
        --peripheralPortRegion :: PortIORegion,
        peripheralMemRegion :: MemIORegion1,
        --peripheralPort :: Array.Array IOHandlerIndex PeripheralHandlerPort,
        peripheralMem :: Array.Array IOHandlerIndex PeripheralHandlerMem,
        peripheralDevices :: PeripheralDevices
    }

-------------------------------------------------------------------------------

type MemPairs = [(Uint16, PeripheralMem)]
type MemPageStubs = [(MemOffset, MemOffset)]

emptyPage = 0
emptyHandler = 0

data PagesBuilder = PagesBuilder {
        pageCounter :: IOPageIndex,
        pageStubs :: MemPageStubs,
        memPairs :: MemPairs,
        regionL1 :: [IOPageIndex],
        regionL2 :: [(IOPageIndex, IOPage)]
    } deriving (Show, Eq)


makePageArray :: MemOffset -> MemOffset -> MemPairs -> [IOHandlerIndex] -> [IOHandlerIndex]
makePageArray memOffset end [] indexes =
    indexes ++ replicate (end - memOffset) emptyHandler
makePageArray memOffset end pairs indexes | memOffset == end =
    indexes
makePageArray memOffset end pairs indexes =
    if memOffset <= s2 then
        makePageArray (memOffset + 1) end pairs (indexes ++ [index])
    else
        makePageArray memOffset end (tail pairs) indexes
    where
        (handlerIndex, (PeripheralMem (s1, s2) _)) = head pairs
        index = if memOffset < s1 then emptyHandler else handlerIndex


makePage :: MemOffset -> MemOffset -> MemPairs -> (MemPairs, IOPage)
makePage start end pairs = (remain, page)
    where
        covered = takeWhile startInPage pairs
        remain = dropWhile endOutsidePage pairs
        startInPage = (<= end) . fst . peripheralMemLoc . snd
        endOutsidePage = (<= end) . snd . peripheralMemLoc . snd
        indexes = makePageArray start end covered []
        page = UArray.listArray (0, (end - start)) indexes


makeMemP :: PagesBuilder -> PagesBuilder
makeMemP b@(PagesBuilder _ [] _ _ _) = b
makeMemP b@(PagesBuilder _ stubs [] l1 _) =
    b { pageStubs = [], regionL1 = l1_ }
    where
        l1_ = l1 ++ replicate (length stubs) emptyPage
makeMemP (PagesBuilder counter stubs pairs@(pairHead:_) l1 l2) = 
    if null occupiedStubs then
        makeMemP $ PagesBuilder counter [] pairs l1_ l2
        else
            let
                newCounter = counter + 1
                ((start, end) : newStubs) = occupiedStubs
                (newPairs, page) = makePage start end pairs
                newL1 = l1_ ++ [newCounter]
                newL2 = l2 ++ [(newCounter, page)]
                in
            makeMemP $ PagesBuilder newCounter newStubs newPairs newL1 newL2
    where
        (nextMemStart, _) = peripheralMemLoc . snd $ pairHead
        (emptyStubs, occupiedStubs) = span ((< nextMemStart) . snd) stubs
        l1_ = l1 ++ map (\_ -> emptyPage) emptyStubs
        newCounter = counter + 1


createPeripherals :: PeripheralDevices 
                     -> Int
                     -> Int
                     -> [PeripheralPort] 
                     -> [PeripheralMem] 
                     -> Peripheral
createPeripherals devices memSize pageSize portEntries memEntries = 
        Peripheral memRegion memHandlers devices
    where
        stubs =
            takeWhile ((<= memSize) . snd) [((i-1) * pageSize, i * pageSize) | i <- [1..]]
        portPairs = 
            zip [1..] $ sortOn (\(PeripheralPort port _) -> port) portEntries
        memPairs = 
            zip [1..] $ sortOn (\(PeripheralMem (start, _) _) -> start) memEntries
        builder = makeMemP $ PagesBuilder 0 stubs memPairs [] []
        memRegion = MemIORegion1 pageSize
                    (UArray.listArray (0, (length stubs)) (regionL1 builder))
                    (Array.array (1, (pageCounter builder)) (regionL2 builder))
        memHandlers = Array.array (1, (fromIntegral $ length memPairs)) 
                      (map (\(i, (PeripheralMem _ handlers)) -> (fromIntegral i, handlers)) memPairs)
