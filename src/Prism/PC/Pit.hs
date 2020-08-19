{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
        pitFormat :: PitFormat, -- BCD or hex
        pitPreset :: Uint16, -- CR
        pitGate :: Bool,
        pitNull :: Bool,
        pitOut :: Bool,
        pitCounter :: Uint16, -- actual counter, CE
        pitStart :: Uint64, -- steady time when counting started
        pitStartCpu :: Uint64, -- CPU cycles when counter started
        pitScale :: Uint16 -- mapping between steady time and counter
    } deriving (Show)

data PitExternal = PitExternal {
        pitExtEnabled :: Bool, -- enables counting
        pitExtRW :: PitModeRW, -- in which order to read/write registers
        pitExtReadQueue :: [PitReadQueueItem], -- latched values for read
        pitExtWriteQueue :: [PitWriteQueueItem], -- latched values for write
        pitExtToWrite :: Uint16, -- value ready to be written to CR
        pitExtCounter :: PitCounter
    } deriving (Show)

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

pitReset :: PitExternal -> PitExternal
pitReset pit =  newPit
    where
        oldPitI = pitExtCounter pit
        newPitI = oldPitI { pitNull = False, pitOut = False, pitCounter = 0, pitStart = 0, pitStartCpu = 0, pitScale = 0, pitPreset = 0}
        newPit = pit { pitExtReadQueue = [], pitExtWriteQueue = [], pitExtToWrite = 0, pitExtCounter = newPitI }

pitConfigure :: PitExternal -> PitModeRW -> PitMode -> PitFormat -> PitExternal
pitConfigure pit modeRw mode format = newPit { pitExtCounter = newPitI, pitExtRW = modeRw }
    where
        newPit = (pitReset pit) 
        newPitI = (pitExtCounter newPit) { pitMode = mode, pitFormat = format }
{-
pitWriteCounter :: PitExternal -> Uint8 -> PitExternal
pitWriteCounter pit val = 
    case (pitExtWriteQueue pit) of
        case h:t -> 
        case [] -> 
        -}

-------------------------------------------------------------------------------
