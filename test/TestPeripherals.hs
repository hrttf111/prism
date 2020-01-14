module TestPeripherals where

import Test.Hspec

import Data.List (partition, sortOn, zip, takeWhile)

import Prism
import PrismCpu
import PrismPeripheral

makePairs :: [(MemOffset, MemOffset)] -> MemPairs
makePairs offsets = pairs
    where
        pairs = zip [1..] 
                $ map (flip PeripheralMem emptyMemHandler)
                $ sortOn fst offsets


pairs1 = makePairs [(1, 2), (4, 4)]
pairs2 = makePairs [(0, 9)]
pairs3 = makePairs [(9, 9)] 
pairs4 = makePairs [(1, 8)]
pairs5 = makePairs [(1, 10)]
pairs6 = makePairs []


testPeripherals =
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
