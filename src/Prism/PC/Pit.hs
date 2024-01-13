{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Prism.PC.Pit where

import Data.Bits

import Prism.Cpu (Uint8, Uint16, Uint64, bcdToHex16, hexToBcd16, PrismInt(..), CpuCycles(..), PrismIRQ(..))
import Prism.Peripherals (SchedId(..))

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

data PitCounterNum = PitCounterNum0
                   | PitCounterNum1
                   | PitCounterNum2
                   deriving (Show)

data PitCounter = PitCounter {
        pitPreset :: Uint16, -- CR
        pitGate :: Bool,
        pitNull :: Bool,
        pitOut :: Bool,
        pitCounter :: Uint16, -- actual counter, CE, used by state machines
        pitStart :: CpuCycles, -- CPU cycles when counter started iteration
        pitNext :: CpuCycles -- time when next event occur
    } deriving (Show)

data PitExternal = forall h. (Show h, PitModeHandler h) => PitExternal {
        pitExtEnabled :: Bool, -- enables counting
        pitExtMode :: PitMode,
        pitExtFormat :: PitFormat, -- BCD or hex
        pitExtRW :: PitModeRW, -- in which order to read/write registers
        pitExtReadQueue :: [PitReadQueueItem], -- latched values for read
        pitExtWriteQueue :: [PitWriteQueueItem], -- latched values for write
        pitExtToWrite :: Uint16, -- value ready to be written to CR
        pitExtModeHandler :: h,
        pitExtScheduled :: CpuCycles, -- next scheduled timeout
        pitExtCurrentOut :: Bool,
        pitExtCounter :: PitCounter
    }

instance Show PitExternal where
    show pit = show $ pitExtCounter pit

-------------------------------------------------------------------------------

isReadBackCommand :: Uint8 -> Bool
isReadBackCommand val = (val .&. 0xC0) == 0xC0

isLatchCommand :: Uint8 -> Bool
isLatchCommand val = (val .&. 0x30) == 0

parseRw :: Uint8 -> PitModeRW
parseRw 0x30 = PitRWBoth
parseRw 0x10 = PitRWLeast
parseRw 0x20 = PitRWMost
parseRw _ = PitRWBoth

parseConfigureCommand :: Uint8 -> (PitCounterNum, PitModeRW, PitMode, PitFormat)
parseConfigureCommand val = (counter, modeRw, mode, format)
    where
        counter = parseLatchCommand val
        modeRw = parseRw $ val .&. 0x30
        mode = PitMode $ shiftR (val .&. 0x0E) 1
        format = PitFormat $ (val .&. 0x01) == 1

parseReadBackCommand :: Uint8 -> (Bool, Bool, [PitCounterNum])
parseReadBackCommand command = (count, status, counters)
    where
        count = (command .&. 0x20) == 0
        status = (command .&. 0x10) == 0
        counters = foldl (\l (ctr, bit) -> if (bit .&. command) /= 0 then (ctr:l) else l) []
                        [(PitCounterNum0, 0x2), (PitCounterNum1, 0x4), (PitCounterNum2, 0x8)]

parseLatchCommand :: Uint8 -> PitCounterNum
parseLatchCommand val =
    case val .&. 0xC0 of
        0 -> PitCounterNum0
        1 -> PitCounterNum1
        _ -> PitCounterNum2

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

convertCounterToCycles :: Uint16 -> CpuCycles
convertCounterToCycles val = CpuCycles $ (fromIntegral val) * 4

convertCyclesToCounter :: CpuCycles -> Uint16
convertCyclesToCounter (CpuCycles val) = fromIntegral $ div val 4

pitConvertToHex :: PitFormat -> Uint16 -> Uint16
pitConvertToHex (PitFormat True) val = bcdToHex16 val
pitConvertToHex _ val = val

pitConvertFromHex :: PitFormat -> Uint16 -> Uint16
pitConvertFromHex (PitFormat True) val = hexToBcd16 val
pitConvertFromHex _ val = val

-------------------------------------------------------------------------------

class PitModeHandler h where
    pitModeConfigureCommand :: h -> PitCounter -> CpuCycles -> PitCounter
    pitModeConfigureCounter :: h -> PitCounter -> CpuCycles -> Uint16 -> PitCounter
    pitModeSetGate :: h -> PitCounter -> CpuCycles -> Bool -> PitCounter
    pitModeEvent :: h -> PitCounter -> CpuCycles -> PitCounter

data PitMode0 = PitMode0 deriving (Show, Eq)

instance PitModeHandler PitMode0 where
    pitModeConfigureCommand _ pit time =
        pit { pitOut = False }
    pitModeConfigureCounter _ pit time preset =
        pit { pitNull = True, pitStart = time, pitNext = next, pitPreset = preset, pitCounter = counter }
        where
            next = if pitGate pit then 0 else (time + convertCounterToCycles preset)
            counter = if pitGate pit then preset else 0
    pitModeSetGate _ pit time gate =
        if pitNull pit then
            pit { pitGate = gate, pitNext = next, pitCounter = counter }
            else
                pit { pitGate = gate }
        where
            counter = if pitGate pit then convertCyclesToCounter(pitNext pit - time) else 0
            next = if pitGate pit then 0 else (time + convertCounterToCycles(pitCounter pit))
    pitModeEvent _ pit time =
        pit { pitOut = True, pitNext = 0 }

data PitMode1 = PitMode1 deriving (Show, Eq)

instance PitModeHandler PitMode1 where
    pitModeConfigureCommand _ pit time =
        pit { pitOut = True, pitNull = False }
    pitModeConfigureCounter _ pit time preset =
        pit { pitNull = True, pitNext = 0, pitPreset = preset, pitCounter = 0 }
    pitModeSetGate _ pit time gate =
        pit { pitGate = gate, pitStart = start, pitNext = next, pitOut = False }
        where
            trigger = ((pitGate pit) == False) && (gate == True) -- rising edge of Gate
            next = if trigger then (time + convertCounterToCycles (pitPreset pit)) else (pitNext pit)
            start = if trigger then time else pitStart pit
    pitModeEvent _ pit time =
        pit { pitOut = True, pitNext = 0 }

data PitMode2 = PitMode2 deriving (Show, Eq)

instance PitModeHandler PitMode2 where
    pitModeConfigureCommand _ pit time =
        pit { pitOut = True, pitNull = False }
    pitModeConfigureCounter _ pit time preset =
        pit { pitOut = True, pitNull = True, pitStart = start, pitNext = next, pitPreset = preset, pitCounter = counter }
        where
            next = if pitNull pit == False then -- will be loaded in two cases: after command and on next event
                if pitGate pit then 0 else (time + convertCounterToCycles (preset-1))
                    else
                        pitNext pit
            counter = if pitGate pit then preset else 0
            start = if pitNull pit then pitStart pit else time
    pitModeSetGate _ pit time gate = pit
    pitModeEvent m pit time =
        if pitOut pit then
            let next = time + convertCounterToCycles 1
            in
            pit { pitOut = False, pitNext = next }
            else
                pitModeConfigureCounter m (pit {pitNull = False}) time (pitPreset pit)

data PitMode3 = PitMode3 deriving (Show, Eq)

instance PitModeHandler PitMode3 where
    pitModeConfigureCommand _ pit time =
        pit { pitOut = True, pitNull = False }
    pitModeConfigureCounter _ pit time preset =
        pit { pitNull = True, pitStart = start, pitNext = next, pitPreset = preset, pitCounter = counter }
        where
            isOdd = (==1) $ mod preset 2
            firstHalfNext = if isOdd then div (preset + 1) 2 else div preset 2
            secondHalfNext = if isOdd then div (preset - 1) 2 else firstHalfNext
            firstHalf = pitOut pit
            mkNextCycles n = time + convertCounterToCycles n
            next = if pitNull pit == False then -- will be loaded in two cases: after command and on next event
                       if firstHalf then mkNextCycles firstHalfNext else mkNextCycles secondHalfNext
                           else
                               pitNext pit
            counter = if pitGate pit then preset else 0
            start = if pitNull pit then pitStart pit else time
    pitModeSetGate _ pit time gate = pit
    pitModeEvent m pit time =
        if pitOut pit then -- is first half
                pitModeConfigureCounter m (pit {pitNull = False, pitOut = False}) time (pitPreset pit)
            else
                pitModeConfigureCounter m (pit {pitNull = False, pitOut = True}) time (pitPreset pit)

data PitMode4 = PitMode4 deriving (Show, Eq)

instance PitModeHandler PitMode4 where
    pitModeConfigureCommand _ pit time =
        pit { pitOut = True, pitNull = False }
    pitModeConfigureCounter _ pit time preset =
        pit { pitOut = True, pitNull = True, pitStart = time, pitNext = next, pitPreset = preset, pitCounter = counter }
        where
            next = if pitGate pit then 0 else (time + convertCounterToCycles (preset-1))
            counter = if pitGate pit then preset else 0
    pitModeSetGate _ pit time gate = pit
    pitModeEvent m pit time =
        if pitOut pit then
            let next = time + convertCounterToCycles 1
            in
            pit { pitOut = False, pitNext = next }
            else
                pit { pitOut = True, pitNext = 0 }

data PitMode5 = PitMode5 deriving (Show, Eq)

instance PitModeHandler PitMode5 where
    pitModeConfigureCommand _ pit time =
        pit { pitOut = True, pitNull = False, pitCounter = 0, pitNext = 0 }
    pitModeConfigureCounter _ pit time preset =
        pit { pitNull = True, pitPreset = preset }
    pitModeSetGate _ pit time gate =
        pit { pitNull = True, pitStart = time, pitNext = next, pitCounter = 0 }
        where
            preset = pitPreset pit
            next = time + convertCounterToCycles (preset-1)
    pitModeEvent m pit time =
        if pitOut pit then
            let next = time + convertCounterToCycles 1
            in
            pit { pitOut = False, pitNext = next }
            else
                pit { pitOut = True, pitNext = 0 }

pitCMode :: forall h. (Show h, PitModeHandler h) => h -> PitExternal -> PitExternal
pitCMode h pit = PitExternal (pitExtEnabled pit)
                                         (pitExtMode pit)
                                         (pitExtFormat pit)
                                         (pitExtRW pit)
                                         (pitExtReadQueue pit)
                                         (pitExtWriteQueue pit)
                                         (pitExtToWrite pit)
                                         h
                                         (pitExtScheduled pit)
                                         (pitExtCurrentOut pit)
                                         (pitExtCounter pit)

pitSetModeHandler :: PitMode -> PitExternal -> PitExternal
pitSetModeHandler (PitMode 0) pit = pitCMode PitMode0 pit
pitSetModeHandler (PitMode 1) pit = pitCMode PitMode1 pit
pitSetModeHandler (PitMode 2) pit = pitCMode PitMode2 pit
pitSetModeHandler (PitMode 3) pit = pitCMode PitMode3 pit
pitSetModeHandler (PitMode 4) pit = pitCMode PitMode4 pit
pitSetModeHandler (PitMode 5) pit = pitCMode PitMode5 pit

pitModeConfigureCommand_ :: PitExternal -> CpuCycles -> PitExternal
pitModeConfigureCommand_ pit@(PitExternal _ _ _ _ _ _ _ h _ _ counter) time =
    pit { pitExtCounter = pitModeConfigureCommand h counter time }

pitModeConfigureCounter_ :: PitExternal -> CpuCycles -> Uint16 -> PitExternal
pitModeConfigureCounter_ pit@(PitExternal _ _ _ _ _ _ _ h _ _ counter) time preset =
    pit { pitExtCounter = pitModeConfigureCounter h counter time preset }

pitModeSetGate_ :: PitExternal -> CpuCycles -> Bool -> PitExternal
pitModeSetGate_ pit@(PitExternal _ _ _ _ _ _ _ h _ _ counter) time gate =
    pit { pitExtCounter = pitModeSetGate h counter time gate }

pitModeEvent_ :: PitExternal -> CpuCycles -> PitExternal
pitModeEvent_ pit@(PitExternal _ _ _ _ _ _ _ h _ _ counter) time =
    pit { pitExtCounter = pitModeEvent h counter time }

pitCountBackwards :: CpuCycles -> CpuCycles -> Uint16 -> CpuCycles
pitCountBackwards (CpuCycles current) (CpuCycles start) max =
    CpuCycles $ mod (current - start) $ fromIntegral max

-------------------------------------------------------------------------------

pitGetCurrentCounter :: PitExternal -> CpuCycles -> Uint16
pitGetCurrentCounter pit time = pitConvertFromHex (pitExtFormat pit) ctr
    where
        counter = pitExtCounter pit
        (PitFormat bcd) = pitExtFormat pit
        max = if pitExtFormat pit == PitFormat True then 9999 else 0xFFFF
        diff = (pitPreset counter) - (convertCyclesToCounter $ pitCountBackwards time (pitStart counter) max)
        ctr = if pitCounter counter > 0 then pitCounter counter else diff

pitGetCurrentLeast :: PitExternal -> CpuCycles -> Uint8
pitGetCurrentLeast pit time = least
    where
        counter = pitGetCurrentCounter pit time
        least = fromIntegral counter

pitGetCurrentMost :: PitExternal -> CpuCycles -> Uint8
pitGetCurrentMost pit time = most
    where
        counter = pitGetCurrentCounter pit time
        most = fromIntegral $ shiftR counter 8

pitReset :: PitExternal -> PitExternal
pitReset pit =
    pit { pitExtReadQueue = [], pitExtWriteQueue = [], pitExtToWrite = 0, pitExtCounter = newCounter }
    where
        newCounter = pitEmpty { pitGate = (pitGate . pitExtCounter $ pit) }

-------------------------------------------------------------------------------

pitConfigure :: PitExternal -> CpuCycles -> PitModeRW -> PitMode -> PitFormat -> PitExternal
pitConfigure pit time modeRw mode format =
    newPit { pitExtMode = mode, pitExtFormat = format, pitExtRW = modeRw, pitExtWriteQueue = pitFillWriteQueue $ modeRw  }
    where
        newPit = pitModeConfigureCommand_ (pitSetModeHandler mode $ (pitReset pit)) time

pitLatchCounter :: PitExternal -> CpuCycles -> PitExternal
pitLatchCounter pit time =
    pit { pitExtReadQueue = rlist }
    where
        canLatch = null $ pitExtReadQueue pit
        llist = if canLatch then
                    case pitExtRW pit of
                        PitRWLeast -> [PitLatchLeast $ pitGetCurrentLeast pit time]
                        PitRWMost -> [PitLatchMost $ pitGetCurrentMost pit time]
                        PitRWBoth -> [PitLatchLeast $ pitGetCurrentLeast pit time
                                     , PitLatchMost $ pitGetCurrentMost pit time]
                    else []
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
                           (pitExtMode pit)
                           (pitExtFormat pit)

pitLatch :: PitExternal -> CpuCycles -> Bool -> Bool -> PitExternal
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

pitWriteCounter :: PitExternal -> CpuCycles -> Uint8 -> PitExternal
pitWriteCounter pit cpuTime val = 
    case (pitExtWriteQueue pit) of
        (h:t) ->
            let preset = pitUpdateToWrite h val $ pitExtToWrite pit
            in
            if null t then
                    let presetHex = pitConvertToHex (pitExtFormat pit) preset
                        pit' = pitModeConfigureCounter_ pit cpuTime presetHex
                    in
                    pit' { pitExtToWrite = 0, pitExtWriteQueue = pitFillWriteQueue $ pitExtRW pit' }
                else
                    pit { pitExtToWrite = preset, pitExtWriteQueue = t }
        [] -> pit

pitReadCounter :: PitExternal -> CpuCycles -> (Uint8, PitExternal)
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

pitEmpty :: PitCounter
pitEmpty =
    PitCounter 0 False False False 0 0 0

pitExtEmpty :: PitExternal
pitExtEmpty =
    PitExternal True (PitMode 0) (PitFormat False) PitRWLeast [] [PitWriteLeast] 0 PitMode0 (CpuCycles 0) False pitEmpty

setPitCounter :: Pit -> PitCounterNum -> PitExternal -> Pit
setPitCounter pit PitCounterNum0 counter =
    pit { pitCounter0 = counter }
setPitCounter pit PitCounterNum1 counter =
    pit { pitCounter1 = counter }
setPitCounter pit PitCounterNum2 counter =
    pit { pitCounter2 = counter }

getPitCounter :: Pit -> PitCounterNum -> PitExternal
getPitCounter pit PitCounterNum0 = pitCounter0 pit
getPitCounter pit PitCounterNum1 = pitCounter1 pit
getPitCounter pit PitCounterNum2 = pitCounter2 pit

pitTimer1IRQ = PrismIRQ 255
pitTimer2IRQ = PrismIRQ 255

getPitIrqAndSchedId :: PitCounterNum -> (PrismIRQ, SchedId)
getPitIrqAndSchedId PitCounterNum0 = (PrismIRQ 0, SchedId 0)
getPitIrqAndSchedId PitCounterNum1 = (pitTimer1IRQ, SchedId 1)
getPitIrqAndSchedId PitCounterNum2 = (pitTimer2IRQ, SchedId 2)

-------------------------------------------------------------------------------

data Pit = Pit {
        pitCounter0 :: PitExternal,
        pitCounter1 :: PitExternal,
        pitCounter2 :: PitExternal
    } deriving (Show)

defaultPIT = Pit pitExtEmpty pitExtEmpty pitExtEmpty

pitControlCommand :: Pit -> CpuCycles -> Uint8 -> Pit
pitControlCommand pit time command | isReadBackCommand command =
    foldl process pit counters
    where
        (count, status, counters) = parseReadBackCommand command
        process p ctr =
            setPitCounter p ctr $ pitLatch (getPitCounter p ctr) time count status
pitControlCommand pit time command | isLatchCommand command =
    let counter = parseLatchCommand command
    in
    setPitCounter pit counter $ pitLatchCounter (getPitCounter pit counter) time
pitControlCommand pit time command =
    setPitCounter pit counter $ pitConfigure (getPitCounter pit counter) time modeRw mode format
    where
        (counter, modeRw, mode, format) = parseConfigureCommand command

pitWrite :: Pit -> PitCounterNum -> CpuCycles -> Uint8 -> Pit
pitWrite pit counter time val =
    setPitCounter pit counter $ pitWriteCounter (getPitCounter pit counter) time val

pitRead :: Pit -> PitCounterNum -> CpuCycles -> (Uint8, Pit)
pitRead pit counter time =
    let (val, pitE) = pitReadCounter (getPitCounter pit counter) time
    in
    (val, setPitCounter pit counter pitE)

pitSetGate :: Pit -> PitCounterNum -> CpuCycles -> Bool -> Pit
pitSetGate pit counter time gate =
    setPitCounter pit counter $ pitModeSetGate_ (getPitCounter pit counter) time gate

pitSetEvent :: Pit -> PitCounterNum -> CpuCycles -> Pit
pitSetEvent pit counter time =
    setPitCounter pit counter $ pitModeEvent_ (getPitCounter pit counter) time

data PitAction = PitActionIrq Bool PrismIRQ
               | PitActionScheduleAdd SchedId CpuCycles
               | PitActionScheduleRemove SchedId
               deriving (Show)

pitDoUpdate :: Pit -> CpuCycles -> (Pit, [PitAction])
pitDoUpdate pit time =
    foldl doUpdate (pit, []) [PitCounterNum0, PitCounterNum1, PitCounterNum2]
    where
        doUpdate (p, result) ctr =
            let counter = getPitCounter pit ctr
                out = pitOut . pitExtCounter $ counter
                lastOut = pitExtCurrentOut counter
                (irq, schedId) = getPitIrqAndSchedId ctr
                timeoutCycles = pitNext . pitExtCounter $ counter
                actions =
                    (\l -> if lastOut /= out then
                        (PitActionIrq out irq:l) else l) $
                        (\l -> if time < timeoutCycles then
                            if timeoutCycles == 0 then
                                (PitActionScheduleRemove schedId:l) else
                                    (PitActionScheduleAdd schedId timeoutCycles:l)
                            else l) []
                counter' = counter { pitExtCurrentOut = out, pitExtScheduled = timeoutCycles }
            in
            (setPitCounter p ctr counter', result ++ actions)

-------------------------------------------------------------------------------
