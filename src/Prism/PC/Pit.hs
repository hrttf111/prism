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
                      | PitLatchMost Uint8
                      | PitLatchLeast Uint8
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

instance Show PitExternal where
    show pit = show $ pitExtCounter pit

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

parseLatchCommand :: Uint8 -> Uint8
parseLatchCommand val = shiftR val 6

serializeRW :: PitModeRW -> Uint8
serializeRW PitRWLeast = 0x10
serializeRW PitRWMost = 0x20
serializeRW PitRWBoth = 0x30

serializeMode :: PitMode -> Uint8
serializeMode (PitMode v) = shiftL v 1

serializeStatus :: PitStatus -> Uint8
serializeStatus (PitStatus out null rw mode (PitFormat b)) =
    (if out then 0x80 else 0x00) .|.
    (if null then 0x40 else 0x00) .|.
    serializeRW rw .|.
    serializeMode mode .|.
    (if b then 0x01 else 0x00)

convertCounterToTime :: Uint16 -> Uint64
convertCounterToTime val = (fromIntegral val) * 4

convertTimeToCounter :: Uint64 -> Uint16
convertTimeToCounter val = fromIntegral $ div val 4

-------------------------------------------------------------------------------

class PitModeHandler h where
    pitModeConfigureCommand :: h -> PitCounter -> Uint64 -> PitCounter
    pitModeConfigureCounter :: h -> PitCounter -> Uint64 -> Uint16 -> PitCounter
    pitModeSetGate :: h -> PitCounter -> Uint64 -> Bool -> PitCounter
    pitModeEvent :: h -> PitCounter -> Uint64 -> PitCounter

data PitMode0 = PitMode0 deriving (Show, Eq)

instance PitModeHandler PitMode0 where
    pitModeConfigureCommand _ pit time =
        pit { pitOut = False }
    pitModeConfigureCounter _ pit time preset =
        pit { pitNull = True, pitStart = time, pitNext = next, pitPreset = preset, pitCounter = counter }
        where
            next = if pitGate pit then 0 else (time + convertCounterToTime preset)
            counter = if pitGate pit then preset else 0
    pitModeSetGate _ pit time gate =
        if pitStart pit == 0 then
            pit { pitGate = gate }
            else
                pit { pitGate = gate, pitNext = next, pitCounter = counter }
        where
            counter = if pitGate pit then convertTimeToCounter(pitNext pit - time) else 0
            next = if pitGate pit then 0 else (time + convertCounterToTime(pitCounter pit))
    pitModeEvent _ pit time =
        pit { pitOut = True, pitStart = 0, pitNext = 0 }

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

pitModeConfigureCounter_ :: PitExternal -> Uint64 -> Uint16 -> PitExternal
pitModeConfigureCounter_ pit@(PitExternal _ _ _ _ _ h counter) time preset =
    pit { pitExtCounter = pitModeConfigureCounter h counter time preset }

pitModeSetGate_ :: PitExternal -> Uint64 -> Bool -> PitExternal
pitModeSetGate_ pit@(PitExternal _ _ _ _ _ h counter) time gate =
    pit { pitExtCounter = pitModeSetGate h counter time gate }

pitModeEvent_ :: PitExternal -> Uint64 -> PitExternal
pitModeEvent_ pit@(PitExternal _ _ _ _ _ h counter) time =
    pit { pitExtCounter = pitModeEvent h counter time }

-------------------------------------------------------------------------------

pitGetCurrentCounter :: PitExternal -> Uint64 -> Uint16
pitGetCurrentCounter pit time = counter
    where
        pitI = pitExtCounter pit
        diff = time - pitStart pitI
        counter = if pitCounter pitI > 0 then pitCounter pitI else convertTimeToCounter diff

pitGetCurrentLeast :: PitExternal -> Uint64 -> Uint8
pitGetCurrentLeast pit time = least
    where
        counter = pitGetCurrentCounter pit time
        least = fromIntegral counter

pitGetCurrentMost :: PitExternal -> Uint64 -> Uint8
pitGetCurrentMost pit time = most
    where
        counter = pitGetCurrentCounter pit time
        most = fromIntegral $ shiftR counter 8

pitReset :: PitExternal -> PitExternal
pitReset pit =  newPit
    where
        oldPitI = pitExtCounter pit
        newPitI = oldPitI { pitNull = False, pitOut = False, pitCounter = 0, pitStart = 0, pitNext = 0, pitPreset = 0}
        newPit = pit { pitExtReadQueue = [], pitExtWriteQueue = [], pitExtToWrite = 0, pitExtCounter = newPitI }

pitConfigure :: PitExternal -> Uint64 -> PitModeRW -> PitMode -> PitFormat -> PitExternal
pitConfigure pit time modeRw mode format = newPit { pitExtCounter = newPitI, pitExtRW = modeRw, pitExtWriteQueue = pitFillWriteQueue $ modeRw  }
    where
        newPit = pitModeConfigureCommand_ (pitSetModeHandler mode $ (pitReset pit)) time
        newPitI = (pitExtCounter newPit) { pitMode = mode, pitFormat = format }

pitLatchCounter :: PitExternal -> Uint64 -> PitExternal
pitLatchCounter pit time =
    pit { pitExtReadQueue = rlist }
    where
        canLatch = True -- todo check list content
        llist = case pitExtRW pit of
                    PitRWLeast -> [PitLatchLeast $ pitGetCurrentLeast pit time]
                    PitRWMost -> [PitLatchMost $ pitGetCurrentMost pit time]
                    PitRWBoth -> [PitLatchLeast $ pitGetCurrentLeast pit time
                                 , PitLatchMost $ pitGetCurrentMost pit time]
        rlist = (pitExtReadQueue pit) ++ llist

pitLatchStatus :: PitExternal -> PitExternal
pitLatchStatus pit =
    pit { pitExtReadQueue = rlist }
    where
        pitI = pitExtCounter pit
        rlist = (pitExtReadQueue pit) ++ [PitLatchStats status]
        status = PitStatus (pitOut pitI)
                           (pitNull pitI)
                           (pitExtRW pit)
                           (pitMode pitI)
                           (pitFormat pitI)

pitLatch :: PitExternal -> Uint64 -> Bool -> Bool -> PitExternal
pitLatch pit time latchCount latchStatus =
    pit''
    where
        pit' = if latchCount then pitLatchCounter pit time else pit
        pit'' = if latchStatus then pitLatchStatus pit' else pit'

pitUpdateToWrite :: PitWriteQueueItem -> Uint8 -> Uint16 -> Uint16
pitUpdateToWrite PitWriteLeast newVal val = (val .&. 0xFF00) .|. (fromIntegral newVal)
pitUpdateToWrite PitWriteMost newVal val = (val .&. 0x00FF) .|. (shiftL (fromIntegral newVal) 8)

pitFillWriteQueue :: PitModeRW -> [PitWriteQueueItem]
pitFillWriteQueue PitRWLeast = [PitWriteLeast]
pitFillWriteQueue PitRWMost = [PitWriteMost]
pitFillWriteQueue PitRWBoth = [PitWriteLeast, PitWriteMost]

pitEvent :: PitExternal -> Uint64 -> PitExternal
pitEvent pit time =
    pitModeEvent_ pit time

pitSetGate :: PitExternal -> Uint64 -> Bool -> PitExternal
pitSetGate pit time gate =
    pitModeSetGate_ pit time gate

pitWriteCounter :: PitExternal -> Uint64 -> Uint8 -> PitExternal
pitWriteCounter pit cpuTime val = 
    case (pitExtWriteQueue pit) of
        (h:t) ->
            let preset = pitUpdateToWrite h val $ pitExtToWrite pit
            in
            if null t then
                    let pit' = pitModeConfigureCounter_ pit cpuTime preset
                    in
                    pit' { pitExtToWrite = 0, pitExtWriteQueue = pitFillWriteQueue $ pitExtRW pit' }
                else
                    pit { pitExtToWrite = preset, pitExtWriteQueue = t }
        [] -> pit

pitReadCounter :: PitExternal -> Uint64 -> (Uint8, PitExternal)
pitReadCounter pit time =
    case (pitExtReadQueue pit) of
        (PitReadMost:t) -> (pitGetCurrentMost pit time, pit { pitExtReadQueue = t } )
        (PitReadLeast:t) -> (pitGetCurrentLeast pit time, pit { pitExtReadQueue = t } )
        ((PitLatchMost v):t) -> (v, pit { pitExtReadQueue = t })
        ((PitLatchLeast v):t) -> (v, pit { pitExtReadQueue = t })
        ((PitLatchStats s):t) -> (serializeStatus s, pit { pitExtReadQueue = t })
        [] -> case pitExtRW pit of
            PitRWLeast -> (pitGetCurrentLeast pit time, pit)
            PitRWMost -> (pitGetCurrentMost pit time, pit)
            PitRWBoth -> (pitGetCurrentLeast pit time, pit { pitExtReadQueue = [PitReadMost] } )

pitEmpty :: PitExternal
pitEmpty =
    PitExternal True PitRWLeast [] [PitWriteLeast] 0 PitMode0
                (PitCounter (PitMode 0) (PitFormat False) 0 False False False 0 0 0)

data Pit = Pit {
        pit0 :: PitExternal,
        pit0Scheduled :: Uint64,
        pit0Level :: Bool
    } deriving (Show)

defaultPIT = Pit pitEmpty 0 False

-------------------------------------------------------------------------------

pitControlCommand :: Pit -> Uint64 -> Uint8 -> Pit
pitControlCommand pit time command | isReadBackCommand command =
    pit { pit0 = pitE }
    where
        (count, status, counters) = parseReadBackCommand command
        pitE = pitLatch (pit0 pit) time count status
pitControlCommand pit time command | isLatchCommand command =
    pit { pit0 = pitE }
    where
        counter = parseLatchCommand command
        pitE = pitLatchCounter (pit0 pit) time
pitControlCommand pit time command =
    pit { pit0 = pitE }
    where
        (counter, modeRw, mode, format) = parseConfigureCommand command
        pitE = pitConfigure (pit0 pit) time modeRw mode format

pitWrite :: Pit -> Uint64 -> Uint8 -> Pit
pitWrite pit time val =
    pit { pit0 = pitE }
    where
        pitE = pitWriteCounter (pit0 pit) time val

pitRead :: Pit -> Uint64 -> (Uint8, Pit)
pitRead pit time =
    (val, pit { pit0 = pitE })
    where
        (val, pitE) = pitReadCounter (pit0 pit) time

pitSGate :: Pit -> Uint64 -> Bool -> Pit
pitSGate pit time gate =
    pit { pit0 = pitE }
    where
        pitE = pitSetGate (pit0 pit) time gate

pitSEvent :: Pit -> Uint64 -> Pit
pitSEvent pit time =
    pit { pit0 = pitE }
    where
        pitE = pitEvent (pit0 pit) time

-------------------------------------------------------------------------------
