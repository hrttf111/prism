module TestPeripherals where

import Test.Hspec

import Data.List (partition, sortOn, zip, takeWhile)
import qualified Data.Array as Array
import qualified Data.Array.Unboxed as UArray

import Prism.Cpu
import Prism.Peripherals

-------------------------------------------------------------------------------

makePairs :: Uint16 -> [(MemOffset, MemOffset)] -> MemPairs
makePairs start offsets = zip [start..] 
                        $ map (uncurry MemLocation)
                        $ sortOn fst offsets

makeMemPeripherals :: [MemLocation] -> [PeripheralMem IO]
makeMemPeripherals offsets = map (flip PeripheralMem emptyMemHandler) offsets

makePortPeripherals = id

pairs1 = makePairs 1 [(1, 2), (4, 4)]
pairs2 = makePairs 1 [(0, 9)]
pairs3 = makePairs 1 [(9, 9)] 
pairs4 = makePairs 1 [(1, 8)]
pairs5 = makePairs 1 [(1, 10)]
pairs6 = makePairs 1 []
pairs7 = makePairs 1 [(1, 2), (4, 10)]

makeIOPageTest lst = IOPage $ UArray.listArray (0, (length lst)) lst

makeTestPageBuilder :: MemOffset -> Int -> Int -> MemPairs -> PagesBuilder
makeTestPageBuilder start pageSize memSize pairs =
    PagesBuilder 0 stubs pairs [] []
    where
        stubs = takeWhile ((<= memSize) . memLocationEnd) [(MemLocation ((i-1) * pageSize) (i * pageSize)) | i <- [1..]]

-------------------------------------------------------------------------------

testPeripherals = do
    describe "Test 1" $ do
        it "1" $ do
            (makePageArray 0 10 pairs1 []) `shouldBe` [0, 1, 1, 0, 2, 0, 0, 0, 0, 0]
        it "2" $ do
            (makePageArray 0 10 pairs2 []) `shouldBe` [1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
        it "3" $ do
            (makePageArray 0 10 pairs3 []) `shouldBe` [0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
        it "4" $ do
            (makePageArray 5 10 pairs4 []) `shouldBe` [1, 1, 1, 1, 0]
        it "5" $ do
            (makePageArray 0 5 pairs5 []) `shouldBe` [0, 1, 1, 1, 1]
        it "6" $ do
            (makePageArray 0 10 pairs6 []) `shouldBe` [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    describe "Test makePage" $ do
        it "1" $ do
            (makePage 0 10 pairs1) `shouldBe` ([], makeIOPageTest [0, 1, 1, 0, 2, 0, 0, 0, 0, 0])
        it "2" $ do
            (makePage 0 10 pairs2) `shouldBe` ([], makeIOPageTest [1, 1, 1, 1, 1, 1, 1, 1, 1, 1])
        it "3" $ do
            (makePage 0 10 pairs3) `shouldBe` ([], makeIOPageTest [0, 0, 0, 0, 0, 0, 0, 0, 0, 1])
        it "4" $ do
            (makePage 5 10 pairs4) `shouldBe` ([], makeIOPageTest [1, 1, 1, 1, 0])
        it "5" $ do
            (makePage 0 5 pairs5) `shouldBe` (pairs5, makeIOPageTest [0, 1, 1, 1, 1])
        it "6" $ do
            (makePage 0 10 pairs6) `shouldBe` ([], makeIOPageTest [0, 0, 0, 0, 0, 0, 0, 0, 0, 0])
        it "7" $ do
            (makePage 0 8 pairs7) `shouldBe` (makePairs 2 [(4, 10)], makeIOPageTest [0, 1, 1, 0, 2, 2, 2, 2])
    describe "Test makeMemPages" $ do
        it "Two first pages occupied, multi pages mem handler" $ do
            let pairs = makePairs 1 [(1, 2), (4, 12)]
                result = makeMemP $ makeTestPageBuilder 0 10 100 pairs 
            (pageCounter result) `shouldBe` (2)
            (pageStubs result) `shouldBe` []
            (memPairs result) `shouldBe` []
            (regionL1 result) `shouldBe` [1, 2, 0, 0, 0, 0, 0, 0, 0, 0]
        it "One page in the middle" $ do
            let pairs = makePairs 1 [(30, 32)]
                result = makeMemP $ makeTestPageBuilder 0 10 100 pairs 
            (pageCounter result) `shouldBe` (2)
            (pageStubs result) `shouldBe` []
            (memPairs result) `shouldBe` []
            (regionL1 result) `shouldBe` [0, 0, 1, 2, 0, 0, 0, 0, 0, 0]
        it "One page at the end" $ do
            let pairs = makePairs 1 [(98, 99)]
                result = makeMemP $ makeTestPageBuilder 0 10 100 pairs 
            (pageCounter result) `shouldBe` (1)
            (pageStubs result) `shouldBe` []
            (memPairs result) `shouldBe` []
            (regionL1 result) `shouldBe` [0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
        it "No pairs" $ do
            let pairs = []
                result = makeMemP $ makeTestPageBuilder 0 10 100 pairs 
            (pageCounter result) `shouldBe` (0)
            (pageStubs result) `shouldBe` []
            (memPairs result) `shouldBe` []
            (regionL1 result) `shouldBe` [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    describe "Test createPeripherals" $ do
        it "Read mem handler" $ do
            let p1 = makeMemPeripherals [(MemLocation 8 27), (MemLocation 98 99)]
                p2 = makeMemPeripherals [(MemLocation 28 38), (MemLocation 95 97)]
                devices = 0 :: Int
                (peripheralsR, peripheralsL) = 
                    createPeripheralsLR devices devices 100 10 [] p1 [] p2
                memRegion = peripheralMemRegion peripheralsR
            (findMemIndex memRegion 0) `shouldBe` (emptyHandler)
            (findMemIndex memRegion 100) `shouldBe` (emptyHandler)
            (findMemIndex memRegion 7) `shouldBe` (emptyHandler)
            (findMemIndex memRegion 8) `shouldBe` (1)
            (findMemIndex memRegion 9) `shouldBe` (1)
            (findMemIndex memRegion 10) `shouldBe` (1)
            (findMemIndex memRegion 11) `shouldBe` (1)
            (findMemIndex memRegion 27) `shouldBe` (1)
            (findMemIndex memRegion 98) `shouldBe` (2)
            (findMemIndex memRegion 99) `shouldBe` (2)
            (findMemIndex memRegion 28) `shouldBe` (3)
            (findMemIndex memRegion 38) `shouldBe` (3)
            (findMemIndex memRegion 39) `shouldBe` (emptyHandler)
            (findMemIndex memRegion 95) `shouldBe` (4)
            (findMemIndex memRegion 97) `shouldBe` (4)
    describe "Test makePortList" $ do
        it "makeArray" $ do
            let ports = zip [1..] $ makePortPeripherals [8, 192, 0xFFFE]
                portArray = makePortList ports []
            (length portArray) `shouldBe` 0x10000
            (portArray !! 0) `shouldBe` 0
            (portArray !! 7) `shouldBe` 0
            (portArray !! 8) `shouldBe` 1
            (portArray !! 9) `shouldBe` 0
            (portArray !! 10) `shouldBe` 0
            (portArray !! 192) `shouldBe` 2
            (portArray !! 0xFFFE) `shouldBe` 3
            (portArray !! 0xFFFF) `shouldBe` 0

-------------------------------------------------------------------------------
