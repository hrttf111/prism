{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prism.Peripherals.Scheduler (
        ------------------------------------------------------
        SchedTime (..)
        , SchedId (..)
        , SchedHandler
        , Scheduler (..)
        , schedEventAdd, schedEventRemove
        , expireSched, reschedule
        , emptyScheduler
        ------------------------------------------------------
    ) where

import Data.List (uncons, sortOn, partition)

import Prism.Peripherals.Types

-------------------------------------------------------------------------------

newtype SchedTime = SchedType Int deriving (Show, Eq, Ord, Num)
newtype SchedId = SchedId Int deriving (Show, Eq, Ord)

type SchedHandler p = SchedId -> p -> IO p

data SchedCommand p = SchedEventAdd SchedId SchedTime (SchedHandler p)
                    | SchedEventRemove SchedId

data Event p = Event {
        evId :: SchedId,
        evStart :: SchedTime,
        evExpires :: SchedTime,
        evHandler :: SchedHandler p
    }

data Scheduler p = Scheduler {
        schEvents :: [Event p],
        schCommands :: [SchedCommand p]
    }

emptyScheduler = Scheduler [] []

-------------------------------------------------------------------------------

schedEventAdd :: Scheduler p -> SchedId -> SchedTime -> SchedHandler p -> Scheduler p
schedEventAdd scheduler id timeout handler =
    scheduler { schCommands = commands }
    where
        commands = ((SchedEventAdd id timeout handler) : schCommands scheduler)

schedEventRemove :: Scheduler p -> SchedId -> Scheduler p
schedEventRemove scheduler id =
    scheduler { schCommands = commands }
    where
        commands = ((SchedEventRemove id) : schCommands scheduler)

-------------------------------------------------------------------------------

execCommands :: Scheduler p -> SchedTime -> [Event p]
execCommands scheduler currentTime =
    sortOn evId $
    foldr execCommand (schEvents scheduler) (schCommands scheduler)
    where
        execCommand (SchedEventAdd id timeout handler) events =
            (Event id currentTime (currentTime + timeout) handler) : events
        execCommand (SchedEventRemove id) events =
            filter ((/=id) . evId) events

reschedule :: Scheduler p -> SchedTime -> (Maybe SchedTime, Scheduler p)
reschedule scheduler currentTime =
    (newTime, Scheduler newEvents [])
    where
        newEvents = execCommands scheduler currentTime
        newId = uncons $ newEvents
        newTime = (evExpires . fst) <$> newId

expireSched :: Scheduler p -> SchedTime -> (Maybe SchedTime, [SchedHandler p], Scheduler p)
expireSched scheduler currentTime =
    (newTime, expiredHandlers, Scheduler newEvents [])
    where
        (expiredEvents, newEvents) =
            partition ((<= currentTime) . evExpires)$ execCommands scheduler currentTime
        expiredHandlers = map evHandler expiredEvents
        newTime = (evExpires . fst) <$> (uncons $ newEvents)

-------------------------------------------------------------------------------
