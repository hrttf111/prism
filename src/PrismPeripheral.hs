{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PrismPeripheral where

import Control.Concurrent.STM.TQueue
import Control.Monad.STM (atomically)

import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray
import Data.List (partition, sortOn, zip, takeWhile)

import Prism

-------------------------------------------------------------------------------

data MemLocation = MemLocation {
        memLocationStart :: MemOffset,
        memLocationEnd :: MemOffset
    } deriving (Eq)

instance Show MemLocation where
    show (MemLocation start end) =
        "(" ++ (show start) ++ "," ++ (show end) ++ ")"

data PeripheralHandlerMem p = PeripheralHandlerMem {
        peripheralMemWrite8 :: p -> MemOffset -> Uint8 -> IO p,
        peripheralMemWrite16 :: p -> MemOffset -> Uint16 -> IO p,
        peripheralMemRead8 :: p -> MemOffset -> IO (p, Uint8),
        peripheralMemRead16 :: p -> MemOffset -> IO (p, Uint16)
    }

emptyReadH :: OperandVal b => p -> a -> IO (p, b)
emptyReadH p _ = return (p, 0)

emptyWriteH :: p -> a -> b -> IO p
emptyWriteH p _ _ = return p

emptyMemHandler :: PeripheralHandlerMem p
emptyMemHandler = 
    PeripheralHandlerMem emptyWriteH emptyWriteH emptyReadH emptyReadH

emptyPortHandler :: PeripheralHandlerPort p
emptyPortHandler =
    PeripheralHandlerPort emptyWriteH emptyWriteH emptyReadH emptyReadH

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

data IOCtxLocal p = IOCtxLocal {
        ioCtxMaxRemote :: IOHandlerIndex,
        localRunCounter :: Int,
        localPeripheralPort :: PeripheralArray (PeripheralHandlerPort p),
        localPeripheralMem :: PeripheralArray (PeripheralHandlerMem p),
        localIOQueue :: IOQueue,
        localPeripherals :: p
    }

-------------------------------------------------------------------------------

emptyPage = 0
emptyHandler = 0

type MemPairs = [(Uint16, MemLocation)]

data PagesBuilder = PagesBuilder {
        pageCounter :: IOPageIndex,
        pageStubs :: [MemLocation],
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
        (handlerIndex, MemLocation s1 s2) = head pairs
        index = if memOffset < s1 then emptyHandler else handlerIndex


makePage :: MemOffset -> MemOffset -> MemPairs -> (MemPairs, IOPage)
makePage start end pairs = (remain, page)
    where
        covered = takeWhile startInPage pairs
        remain = dropWhile endOutsidePage pairs
        startInPage = (<= end) . memLocationStart . snd
        endOutsidePage = (<= end) . memLocationEnd . snd
        indexes = makePageArray start end covered []
        page = IOPage $ UArray.listArray (0, (end - start)) indexes


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
                ((MemLocation start end) : newStubs) = occupiedStubs
                (newPairs, page) = makePage start end pairs
                newL1 = l1_ ++ [newCounter]
                newL2 = l2 ++ [(newCounter, page)]
                in
            makeMemP $ PagesBuilder newCounter newStubs newPairs newL1 newL2
    where
        nextMemStart = memLocationStart . snd $ pairHead
        (emptyStubs, occupiedStubs) = span ((< nextMemStart) . memLocationEnd) stubs
        l1_ = l1 ++ map (\_ -> emptyPage) emptyStubs
        newCounter = counter + 1

makePortArray :: [(Uint16, PeripheralPort p )] -> [IOHandlerIndex] -> [IOHandlerIndex]
makePortArray [] indexes = 
    indexes ++ replicate (0x10000 - length indexes) emptyHandler
makePortArray ((index, peripheral):tail) indexes =
    let
        toReplicate = fromIntegral (peripheralPortLoc peripheral) - length indexes
        newIndexes = indexes 
                     ++ replicate toReplicate emptyHandler
                     ++ [index]
        in
    makePortArray tail newIndexes


createPeripherals :: p
                     -> Int
                     -> Int
                     -> [PeripheralPort p] 
                     -> [PeripheralMem p] 
                     -> Peripheral p
createPeripherals devices memSize pageSize portEntries memEntries = 
        Peripheral portRegion memRegion portHandlers memHandlers devices
    where
        stubs =
            takeWhile ((<= memSize) . memLocationEnd) [(MemLocation ((i-1) * pageSize) (i * pageSize)) | i <- [1..]]
        portPairs = 
            zip [1..] $ sortOn (\(PeripheralPort port _) -> port) portEntries
        portRegion =
            PortIORegion $ UArray.listArray (0, 0xFFFF) $ makePortArray portPairs [] 
        portHandlers = 
            Array.array (1, fromIntegral $ length portEntries) 
            $ map (\(i, (PeripheralPort _ h)) -> (i, h)) portPairs
        memPairs = 
            zip [1..] $ sortOn (\(PeripheralMem loc _) -> memLocationStart loc) memEntries
        memPairs1 = map (\(index, PeripheralMem loc _) -> (index, loc)) memPairs
        builder =
            makeMemP $ PagesBuilder 0 stubs memPairs1 [] []
        memRegion = 
            MemIORegion pageSize
                (UArray.listArray (0, (length stubs)) (regionL1 builder))
                (Array.array (1, (pageCounter builder)) (regionL2 builder))
        memHandlers =
            Array.array (1, (fromIntegral $ length memPairs)) 
                (map (\(i, (PeripheralMem _ handlers)) -> (fromIntegral i, handlers)) memPairs)


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

makeEmptyPeripherals :: Int -> p -> Peripheral p
makeEmptyPeripherals memSize devices =
    createPeripherals devices memSize memSize [] [] 

createIOQueue = IOQueue <$> newTQueueIO <*> newTQueueIO

makeEmptyIO :: Int -> p -> IO (IOCtx, Peripheral p)
makeEmptyIO memSize devices = do
    queue <- createIOQueue
    let peripheral = makeEmptyPeripherals memSize devices
        ioCtx = IOCtx queue (peripheralMemRegion peripheral) (peripheralPortRegion peripheral)
    return (ioCtx, peripheral)

-------------------------------------------------------------------------------

execPeripheralsOnce :: IOQueue -> Peripheral p -> IO ()
execPeripheralsOnce queue@(IOQueue req rsp) peripheral = do
    msg <- atomically $ readTQueue req
    putStrLn $ "Got message " ++ (show msg)
    let devices = peripheralDevices peripheral
    peripheralNew <- (case msg of
        IOCmdRead8 IOMemType handlerIndex memOffset -> do
            let handler = (peripheralMem peripheral) Array.! handlerIndex
            (per, val) <- (peripheralMemRead8 handler) devices memOffset
            atomically $ writeTQueue rsp $ IOCmdData8 val
            return $ peripheral { peripheralDevices = per }
        IOCmdRead16 IOMemType handlerIndex memOffset -> do
            let handler = (peripheralMem peripheral) Array.! handlerIndex
            (per, val) <- (peripheralMemRead16 handler) devices memOffset
            atomically $ writeTQueue rsp $ IOCmdData16 val
            return $ peripheral { peripheralDevices = per }
        IOCmdWrite8 IOMemType handlerIndex memOffset val -> do
            let handler = (peripheralMem peripheral) Array.! handlerIndex
            per <- (peripheralMemWrite8 handler) devices memOffset val
            return $ peripheral { peripheralDevices = per }
        IOCmdWrite16 IOMemType handlerIndex memOffset val -> do
            let handler = (peripheralMem peripheral) Array.! handlerIndex
            per <- (peripheralMemWrite16 handler) devices memOffset val
            return $ peripheral { peripheralDevices = per }
        IOCmdRead8 IOPortType handlerIndex memOffset -> do
            let handler = (peripheralPort peripheral) Array.! handlerIndex
            (per, val) <- (peripheralPortRead8 handler) devices $ fromIntegral memOffset
            atomically $ writeTQueue rsp $ IOCmdData8 val
            return $ peripheral { peripheralDevices = per }
        IOCmdRead16 IOPortType handlerIndex memOffset -> do
            let handler = (peripheralPort peripheral) Array.! handlerIndex
            (per, val) <- (peripheralPortRead16 handler) devices $ fromIntegral memOffset
            atomically $ writeTQueue rsp $ IOCmdData16 val
            return $ peripheral { peripheralDevices = per }
        IOCmdWrite8 IOPortType handlerIndex memOffset val -> do
            let handler = (peripheralPort peripheral) Array.! handlerIndex
            per <- (peripheralPortWrite8 handler) devices (fromIntegral memOffset) val
            return $ peripheral { peripheralDevices = per }
        IOCmdWrite16 IOPortType handlerIndex memOffset val -> do
            let handler = (peripheralPort peripheral) Array.! handlerIndex
            per <- (peripheralPortWrite16 handler) devices (fromIntegral memOffset) val
            return $ peripheral { peripheralDevices = per }
        _ -> return peripheral
        )
    putStrLn "Thread end"
    return ()
