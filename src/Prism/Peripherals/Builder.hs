module Prism.Peripherals.Builder where

import Data.List (uncons)
import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray
import Data.List (partition, sortOn, zip, takeWhile)

import Prism.Cpu

import Prism.Peripherals.Types
import Prism.Peripherals.Queue

-------------------------------------------------------------------------------

type MemPairs = [(Uint16, MemLocation)]

data PagesBuilder = PagesBuilder {
        pageCounter :: IOPageIndex,
        pageStubs :: [MemLocation],
        memPairs :: MemPairs,
        regionL1 :: [IOPageIndex],
        regionL2 :: [(IOPageIndex, IOPage)]
    } deriving (Show, Eq)

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

createPeripherals :: p
                     -> Int
                     -> Int
                     -> [PeripheralPort m]
                     -> [PeripheralMem m]
                     -> Peripheral m p
createPeripherals devices memSize pageSize portEntries memEntries =
    fst $ createPeripheralsLR devices devStub memSize pageSize portEntries memEntries [] []
    where
        devStub = 0 :: Int

createPeripheralsL :: p
                     -> Int
                     -> Int
                     -> [PeripheralPort m]
                     -> [PeripheralMem m]
                     -> PeripheralLocal m p
createPeripheralsL devices memSize pageSize portEntries memEntries =
    snd $ createPeripheralsLR devStub devices memSize pageSize portEntries memEntries [] []
    where
        devStub = 0 :: Int


createPeripheralsLR :: pR
                     -> pL
                     -> Int
                     -> Int
                     -> [PeripheralPort mR]
                     -> [PeripheralMem mR]
                     -> [PeripheralPort mL]
                     -> [PeripheralMem mL]
                     -> (Peripheral mR pR, PeripheralLocal mL pL)
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

-------------------------------------------------------------------------------

makeEmptyPeripherals :: Int -> p -> Peripheral m p
makeEmptyPeripherals memSize devices =
    createPeripherals devices memSize memSize [] [] 

-------------------------------------------------------------------------------
