{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Prism.Peripherals.Scheduler (
        ------------------------------------------------------
        SchedTime (..)
        , SchedId (..)
        , SchedHandler, SchedHandlerOut
        , Scheduler (..)
        , schedEventAdd, schedEventRemove
        , expireSched, reschedule
        , emptyScheduler
        ------------------------------------------------------
    ) where

import Data.List (uncons, sortOn, partition)

import Prism.Peripherals.Types

-------------------------------------------------------------------------------

newtype SchedTime = SchedTime Int deriving (Show, Eq, Ord, Num)
newtype SchedId = SchedId Int deriving (Show, Eq, Ord)

type SchedHandler m = SchedId -> m ()
type SchedHandlerOut m = m ()

instance Show (SchedHandler p) where
    show _ = "SchedHandler"

instance Eq (SchedHandler p) where
    _ == _ = True
    _ /= _ = False

instance Show (SchedHandlerOut p) where
    show _ = "SchedHandlerOut"

instance Eq (SchedHandlerOut p) where
    _ == _ = True
    _ /= _ = False

data SchedCommand m = SchedEventAdd SchedId SchedTime (SchedHandler m)
                    | SchedEventRemove SchedId
                    deriving (Show, Eq)

data Event m = Event {
        evId :: SchedId,
        evStart :: SchedTime,
        evExpires :: SchedTime,
        evHandler :: SchedHandler m
    } deriving (Show, Eq)

data Scheduler m = Scheduler {
        schEvents :: [Event m],
        schCommands :: [SchedCommand m]
    } deriving (Show, Eq)

emptyScheduler = Scheduler [] []

-------------------------------------------------------------------------------

schedEventAdd :: (Monad m) => Scheduler m -> SchedId -> SchedTime -> SchedHandler m -> Scheduler m 
schedEventAdd scheduler id timeout handler =
    scheduler { schCommands = commands }
    where
        commands = ((SchedEventAdd id timeout handler) : schCommands scheduler)

schedEventRemove :: (Monad m) => Scheduler m -> SchedId -> Scheduler m
schedEventRemove scheduler id =
    scheduler { schCommands = commands }
    where
        commands = ((SchedEventRemove id) : schCommands scheduler)

-------------------------------------------------------------------------------

execCommands :: (Monad m) => Scheduler m -> SchedTime -> [Event m]
execCommands scheduler currentTime =
    sortOn evExpires $
    foldr execCommand (schEvents scheduler) (schCommands scheduler)
    where
        execCommand (SchedEventAdd id timeout handler) events =
            (Event id currentTime (currentTime + timeout) handler) : events
        execCommand (SchedEventRemove id) events =
            filter ((/=id) . evId) events

reschedule :: (Monad m) => Scheduler m -> SchedTime -> (Maybe SchedTime, Scheduler m)
reschedule scheduler currentTime =
    (newTime, Scheduler newEvents [])
    where
        newEvents = execCommands scheduler currentTime
        newId = uncons $ newEvents
        newTime = (evExpires . fst) <$> newId

expireSched :: (Monad m) => Scheduler m -> SchedTime -> (Maybe SchedTime, [SchedHandlerOut m], Scheduler m)
expireSched scheduler currentTime =
    (newTime, expiredHandlers, Scheduler newEvents [])
    where
        (expiredEvents, newEvents) =
            partition ((<= currentTime) . evExpires)$ execCommands scheduler currentTime
        expiredHandlers = map (\e -> (evHandler e) (evId e)) expiredEvents
        newTime = (evExpires . fst) <$> (uncons $ newEvents)

-------------------------------------------------------------------------------
