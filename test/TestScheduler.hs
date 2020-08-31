module TestScheduler where

import Test.Hspec

import Prism.Peripherals

-------------------------------------------------------------------------------

type TestScheduler = Scheduler IO
emptyTestScheduler :: TestScheduler
emptyTestScheduler = emptyScheduler

testHandler :: SchedHandler IO
testHandler _ = return ()

testScheduler = do
    describe "Empty scheduler" $ do
        it "reschedule" $ do
            (reschedule emptyTestScheduler $ SchedTime 0) `shouldBe` (Nothing, emptyTestScheduler)
        it "expireSched" $ do
            (expireSched emptyTestScheduler $ SchedTime 0) `shouldBe` (Nothing, [], emptyTestScheduler)
    describe "Adding event" $ do
        it "Add to empty" $ do
            let sched = schedEventAdd emptyTestScheduler (SchedId 1) (SchedTime 10) testHandler
                (nextTime, sched2) = reschedule sched $ SchedTime 12
                sched3 = schedEventAdd sched2 (SchedId 2) (SchedTime 11) testHandler
                (nextTime4, sched4) = reschedule sched3 $ SchedTime 12
                sched5 = schedEventAdd sched4 (SchedId 3) (SchedTime 8) testHandler
                (nextTime6, sched6) = reschedule sched5 $ SchedTime 12
                (nextTime7, events, _) = expireSched sched6 $ SchedTime 20
            nextTime `shouldBe` (Just 10)
            nextTime4 `shouldBe` (Just 10)
            nextTime6 `shouldBe` (Just 8)
            nextTime7 `shouldBe` Nothing
    describe "Removing event" $ do
        it "Remove empty" $ do
            let sched = schedEventRemove emptyTestScheduler (SchedId 1)
            (reschedule sched $ SchedTime 12) `shouldBe` (Nothing, emptyTestScheduler)
        it "Remove" $ do
            let sched = schedEventAdd emptyTestScheduler (SchedId 1) (SchedTime 10) testHandler
                (_, sched2) = reschedule sched $ SchedTime 12
                sched3 = schedEventRemove sched2 (SchedId 1)
            (reschedule sched3 $ SchedTime 12) `shouldBe` (Nothing, emptyTestScheduler)
        it "Remove reschedule" $ do
            let sched = schedEventAdd emptyTestScheduler (SchedId 1) (SchedTime 10) testHandler
                (nextTime, sched2) = reschedule sched $ SchedTime 12
                sched3 = schedEventAdd sched2 (SchedId 2) (SchedTime 11) testHandler
                (nextTime4, sched4) = reschedule sched3 $ SchedTime 12
                sched5 = schedEventRemove sched4 (SchedId 1)
                (nextTime6, sched6) = reschedule sched5 $ SchedTime 12
            nextTime6 `shouldBe` (Just 11)

-------------------------------------------------------------------------------
