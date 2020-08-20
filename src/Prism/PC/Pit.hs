{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Prism.PC.Pit where

import Data.Bits

import Prism.Cpu (Uint8, Uint16, Uint64, PrismInt(..))

-------------------------------------------------------------------------------

newtype PitMode = PitMode Uint8 deriving (Show, Eq)
newtype PitFormat = PitFormat Bool deriving (Show, Eq)

data PitModeRW = PitRWLeast
               | PitRWMost
               | PitRWBoth
               deriving (Show, Eq)

data PitStatus = PitStatus {
        pitStatusOut :: Bool,
        pitStatusNull :: Bool,
        pitStatusRw :: PitModeRW,
        pitStatusMode :: PitMode,
        pitStatusFormat :: PitFormat
    } deriving (Show, Eq)

data PitReadQueueItem = PitReadMost
                      | PitReadLeast
                      | PitLatchValue Uint16
                      | PitLatchStats PitStatus
                      deriving (Show, Eq)

data PitWriteQueueItem = PitWriteLeast
                       | PitWriteMost
                       deriving (Show, Eq)

-------------------------------------------------------------------------------

data PitCounter = PitCounter {
        pitMode :: PitMode,
        pitFormat :: PitFormat, -- BCD or hex, move it to PitExternal ???
        pitPreset :: Uint16, -- CR
        pitGate :: Bool,
        pitNull :: Bool,
        pitOut :: Bool,
        pitCounter :: Uint16, -- actual counter, CE, used by stats machines
        pitStart :: Uint64, -- CPU cycles when counter started iteration
        pitNext :: Uint64 -- time when next event occur
    } deriving (Show)

data PitExternal = forall h. (Show h, PitModeHandler h) => PitExternal {
        pitExtEnabled :: Bool, -- enables counting
        pitExtRW :: PitModeRW, -- in which order to read/write registers
        pitExtReadQueue :: [PitReadQueueItem], -- latched values for read
        pitExtWriteQueue :: [PitWriteQueueItem], -- latched values for write
        pitExtToWrite :: Uint16, -- value ready to be written to CR
        pitExtModeHandler :: h,
        pitExtCounter :: PitCounter
    }-- deriving (Show)

-------------------------------------------------------------------------------

isReadBackCommand :: Uint8 -> Bool
isReadBackCommand val = (val .&. 0xC0) == 0xC0

isLatchCommand :: Uint8 -> Bool
isLatchCommand val = (val .&. 0x30) == 0

parseCounterNum :: Uint8 -> Uint8
parseCounterNum command = shiftR (command .&. 0xC0) 4

parseRw :: Uint8 -> PitModeRW
parseRw 0x30 = PitRWBoth
parseRw 0x10 = PitRWLeast
parseRw 0x20 = PitRWMost
parseRw _ = PitRWBoth

parseConfigureCommand :: Uint8 -> (Uint8, PitModeRW, PitMode, PitFormat)
parseConfigureCommand val = (counter, modeRw, mode, format)
    where
        counter = parseCounterNum val
        modeRw = parseRw $ val .&. 0x30
        mode = PitMode $ shiftR (val .&. 0x0E) 1
        format = PitFormat $ (val .&. 0x01) == 1

parseReadBackCommand :: Uint8 -> (Bool, Bool, [Uint8])
parseReadBackCommand command = (count, status, counters)
    where
        count = (command .&. 0x20) == 0
        status = (command .&. 0x10) == 0
        counters = []

{-pitWriteCommand :: PitExternal -> Uint8 -> PitExternal
pitWriteCommand pit command | isReadBackCommand command = pitReadBack
pitWriteCommand pit command | isLatchCommand command = pitLatch
pitWriteCommand pit command = pitConfigure
-}

-------------------------------------------------------------------------------

class PitModeHandler h where
    pitModeConfigureCommand :: h -> PitCounter -> Uint64 -> PitCounter
    pitModeConfigureCounter :: h -> PitCounter -> Uint64 -> Uint16 -> PitCounter
    pitModeSetGate :: h -> PitCounter -> Uint64 -> Bool -> PitCounter
    pitModeEvent :: h -> PitCounter -> Uint64 -> PitCounter

data PitMode0 = PitMode0 deriving (Show, Eq)

instance PitModeHandler PitMode0 where
    pitModeConfigureCommand h pit time = pit { pitOut = False }
    pitModeConfigureCounter h pit time preset =
        pit { pitNull = True, pitStart = time, pitNext = (time + fromIntegral preset), pitPreset = preset, pitCounter = 0 }
    pitModeSetGate h pit time gate = pit
    pitModeEvent h pit time = pit { pitOut = True, pitStart = 0 }

pitSetModeHandler :: PitMode -> PitExternal -> PitExternal
pitSetModeHandler mode pit = PitExternal (pitExtEnabled pit)
                                         (pitExtRW pit)
                                         (pitExtReadQueue pit)
                                         (pitExtWriteQueue pit)
                                         (pitExtToWrite pit)
                                         PitMode0
                                         (pitExtCounter pit)

pitModeConfigureCommand_ :: PitExternal -> Uint64 -> PitExternal
pitModeConfigureCommand_ pit@(PitExternal _ _ _ _ _ h counter) time =
    pit { pitExtCounter = pitModeConfigureCommand h counter time }

-------------------------------------------------------------------------------

pitReset :: PitExternal -> PitExternal
pitReset pit =  newPit
    where
        oldPitI = pitExtCounter pit
        newPitI = oldPitI { pitNull = False, pitOut = False, pitCounter = 0, pitStart = 0, pitNext = 0, pitPreset = 0}
        newPit = pit { pitExtReadQueue = [], pitExtWriteQueue = [], pitExtToWrite = 0, pitExtCounter = newPitI }

pitConfigure :: PitExternal -> PitModeRW -> PitMode -> PitFormat -> PitExternal
pitConfigure pit modeRw mode format = newPit { pitExtCounter = newPitI, pitExtRW = modeRw }
    where
        newPit = pitModeConfigureCommand_ (pitSetModeHandler mode $ (pitReset pit)) 0
        newPitI = (pitExtCounter newPit) { pitMode = mode, pitFormat = format }

pitConfigureCounter :: PitCounter -> Uint64 -> Uint16 -> PitCounter
pitConfigureCounter pit cpuTime preset = pit'
    where
        pit' = pit { pitNull = True, pitStart = cpuTime, pitPreset = preset }

pitUpdateToWrite :: PitWriteQueueItem -> Uint8 -> Uint16 -> Uint16
pitUpdateToWrite PitWriteLeast newVal val = (val .&. 0xFF00) .|. (fromIntegral newVal)
pitUpdateToWrite PitWriteMost newVal val = (val .&. 0x00FF) .|. (shiftL (fromIntegral newVal) 8)

pitFillWriteQueue :: PitModeRW -> [PitWriteQueueItem]
pitFillWriteQueue PitRWLeast = [PitWriteLeast]
pitFillWriteQueue PitRWMost = [PitWriteMost]
pitFillWriteQueue PitRWBoth = [PitWriteLeast, PitWriteMost]

pitWriteCounter :: PitExternal -> Uint64 -> Uint8 -> PitExternal
pitWriteCounter pit cpuTime val = 
    case (pitExtWriteQueue pit) of
        (h:t) ->
            let preset = pitUpdateToWrite h val $ pitExtToWrite pit
            in
            if null t then
                    let pitI = pitConfigureCounter (pitExtCounter pit) cpuTime preset
                    in
                    pit { pitExtToWrite = 0, pitExtWriteQueue = pitFillWriteQueue $ pitExtRW pit, pitExtCounter = pitI }
                else
                    pit { pitExtToWrite = preset, pitExtWriteQueue = t }
        [] -> pit

-------------------------------------------------------------------------------
