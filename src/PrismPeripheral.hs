{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module PrismPeripheral where

import Control.Exception (Exception, throwIO)
import Control.Monad.Trans
import Control.Concurrent.STM.TQueue
import Control.Monad.STM (atomically)

import Data.List (uncons)
import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray
import Data.List (partition, sortOn, zip, takeWhile)

import Prism

-------------------------------------------------------------------------------

instance Show MemLocation where
    show (MemLocation start end) =
        "(" ++ (show start) ++ "," ++ (show end) ++ ")"

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

instance IOMem IOCtx where
    ioMemRead (IOCtx i _ _) handler offset =
        ioMemRead i handler offset
    ioMemWrite (IOCtx i _ _) handler offset val =
        ioMemWrite i handler offset val

instance IOPort IOCtx where
    ioPortRead (IOCtx i _ _) handler offset = 
        ioPortRead i handler offset
    ioPortWrite (IOCtx i _ _) handler offset val =
        ioPortWrite i handler offset val

-------------------------------------------------------------------------------

data IOCtxException = IOCtxException deriving Show

instance Exception IOCtxException

instance IOValRemote Uint8 where
    ioValRemoteRead (IOQueue req rsp) cmdType handler offset = liftIO $ do
        atomically $ writeTQueue req $ IOCmdRead8 cmdType handler offset
        val <- atomically $ readTQueue rsp
        case val of
            IOCmdData8 d -> return d
            _ -> throwIO IOCtxException
    ioValRemoteWrite (IOQueue req _) cmdType handler offset val = liftIO $ do
        atomically $ writeTQueue req $ IOCmdWrite8 cmdType handler offset val
    ioValRemoteRespond (IOQueue _ rsp) val = liftIO $ do
        atomically $ writeTQueue rsp $ IOCmdData8 val

instance IOValRemote Uint16 where
    ioValRemoteRead (IOQueue req rsp) cmdType handler offset = liftIO $ do
        atomically $ writeTQueue req $ IOCmdRead16 cmdType handler offset
        val <- atomically $ readTQueue rsp
        case val of
            IOCmdData16 d -> return d
            _ -> throwIO IOCtxException
    ioValRemoteWrite (IOQueue req _) cmdType handler offset val = liftIO $ do
        atomically $ writeTQueue req $ IOCmdWrite16 cmdType handler offset val
    ioValRemoteRespond (IOQueue _ rsp) val = liftIO $ do
        atomically $ writeTQueue rsp $ IOCmdData16 val

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

data PeripheralsInternal = PeripheralsInternal {
        localQueue :: IOQueue
    }

instance IOMem PeripheralsInternal where
    ioMemRead peripherals handler offset =
        ioValRemoteRead (localQueue peripherals) IOMemType handler offset
    ioMemWrite peripherals handler offset val =
        ioValRemoteWrite (localQueue peripherals) IOMemType handler offset val

instance IOPort PeripheralsInternal where
    ioPortRead peripherals handler offset = 
        ioValRemoteRead (localQueue peripherals) IOPortType handler (fromIntegral offset)
    ioPortWrite peripherals handler offset val =
        ioValRemoteWrite (localQueue peripherals) IOPortType handler (fromIntegral offset) val

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


makePortList:: [(IOHandlerIndex, Uint16)] -> [IOHandlerIndex] -> [IOHandlerIndex]
makePortList [] indexes = 
    indexes ++ replicate (0x10000 - length indexes) emptyHandler
makePortList ((index, peripheral):tail) indexes =
    let
        toReplicate = fromIntegral peripheral - length indexes
        newIndexes = indexes 
                     ++ replicate toReplicate emptyHandler
                     ++ [index]
        in
    makePortList tail newIndexes


makePortRegion :: [(IOHandlerIndex, Uint16)] -> PortIORegion
makePortRegion ports =
    PortIORegion $ UArray.listArray (0, 0xFFFF) $ makePortList ports [] 


makeMemRegion :: Int -> [MemLocation] -> MemPairs -> MemIORegion
makeMemRegion pageSize stubs pairs =
        MemIORegion pageSize
            (UArray.listArray (0, (length stubs)) (regionL1 builder))
            (Array.array (1, (pageCounter builder)) (regionL2 builder))
    where
        builder =
            makeMemP $ PagesBuilder 0 stubs pairs [] []


indexHandlers :: IOHandlerIndex
                 -> IOHandlerIndex
                 -> [PeripheralPort p]
                 -> [PeripheralMem p]
                 -> ([(IOHandlerIndex, PeripheralPort p)], [(IOHandlerIndex, PeripheralMem p)])
indexHandlers portStart memStart portEntries memEntries =
    (portPairs, memPairs)
    where
        portPairs = 
            zip [portStart..] $ sortOn peripheralPortLoc portEntries
        memPairs = 
            zip [memStart..] $ sortOn (memLocationStart . peripheralMemLoc) memEntries


makePageStubs :: Int -> Int -> [MemLocation]
makePageStubs memSize pageSize =
    takeWhile ((<= memSize) . memLocationEnd) [(MemLocation ((i-1) * pageSize) (i * pageSize)) | i <- [1..]]


peripheralArrayMem :: [(IOHandlerIndex, PeripheralMem p)] 
                      -> PeripheralArray (PeripheralHandlerMem p)
peripheralArrayMem memPairs =
    Array.array (start, end)
        $ map (\(i, (PeripheralMem _ handlers)) -> (fromIntegral i, handlers)) memPairs
    where
        start = maybe 1 (fst . fst) $ uncons memPairs
        end = start + (fromIntegral $ length memPairs) - 1


peripheralArrayPort :: [(IOHandlerIndex, PeripheralPort p)] 
                      -> PeripheralArray (PeripheralHandlerPort p)
peripheralArrayPort portPairs =
    Array.array (start, end)
        $ map (\(i, (PeripheralPort _ h)) -> (i, h)) portPairs
    where
        start = maybe 1 (fst . fst) $ uncons portPairs
        end = start + (fromIntegral $ length portPairs) - 1


convertPortPairs portPairs =
    map (\(index, PeripheralPort loc _) -> (index, loc)) portPairs

convertMemPairs memPairs =
     map (\(index, PeripheralMem loc _) -> (index, loc)) memPairs


createPeripherals :: p
                     -> Int
                     -> Int
                     -> [PeripheralPort p] 
                     -> [PeripheralMem p] 
                     -> Peripheral p
createPeripherals devices memSize pageSize portEntries memEntries = 
    fst $ createPeripheralsLR devices devStub memSize pageSize portEntries memEntries [] []
    where
        devStub = 0 :: Int


createPeripheralsLR :: pR
                     -> pL
                     -> Int
                     -> Int
                     -> [PeripheralPort pR] 
                     -> [PeripheralMem pR] 
                     -> [PeripheralPort pL] 
                     -> [PeripheralMem pL] 
                     -> (Peripheral pR, PeripheralLocal pL)
createPeripheralsLR devicesR devicesL memSize pageSize portEntriesR memEntriesR portEntriesL memEntriesL = 
        (Peripheral portRegion memRegion portHandlersR memHandlersR devicesR
        , PeripheralLocal portMax memMax portRegion memRegion portHandlersL memHandlersL devicesL)
    where
        stubs = makePageStubs memSize pageSize
        (portPairsR, memPairsR) =
            indexHandlers 1 1 portEntriesR memEntriesR
        portMax = fromIntegral $ length portPairsR
        memMax = fromIntegral $ length memPairsR
        (portPairsL, memPairsL) =
            indexHandlers (portMax+1) (memMax+1) portEntriesL memEntriesL
        portPairsM = 
            sortOn snd
            $ (convertPortPairs portPairsR) ++ (convertPortPairs portPairsL)
        memPairsM =
            sortOn (memLocationStart . snd)
            $ (convertMemPairs memPairsR) ++ (convertMemPairs memPairsL)
        portRegion = makePortRegion portPairsM
        memRegion = makeMemRegion pageSize stubs memPairsM
        portHandlersR = peripheralArrayPort portPairsR
        memHandlersR = peripheralArrayMem memPairsR
        portHandlersL = peripheralArrayPort portPairsL
        memHandlersL = peripheralArrayMem memPairsL


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
        ioCtx = IOCtx (PeripheralsInternal queue) (peripheralMemRegion peripheral) (peripheralPortRegion peripheral)
    return (ioCtx, peripheral)

-------------------------------------------------------------------------------
