{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Peripherals.Pic where

import Control.Monad.Trans

import Data.Bits

import Prism
import PrismPeripheral

-------------------------------------------------------------------------------

data PicReadReg = PicIRR | PicISR deriving (Show, Eq)
data PicICW = PicICW1
              | PicICW2
              | PicICW3
              | PicICW4
              | PicICWDone
              deriving (Show, Eq)

data PicAction = PicNoAction
                 | PicClearInt
                 | PicRaiseInt
                 | PicIntrActive Bool
                 deriving (Show)

data PicCommand = PicRead PicReadReg
                  | PicSMCommand Bool
                  | PicPoll
                  | PicRotateAEOI Bool
                  | PicEOI Bool (Maybe Uint8)
                  | PicSetPrio Uint8
                  | PicICWCommand PicICW Uint8
                  | PicSetIMR Uint8
                  deriving (Show)


data PicConfig = PicConfig {
    picConfigLevel :: Bool,
    picConfigSingle :: Bool,
    picConfigADI :: Uint8,
    picConfigAddr :: Uint8,
    picConfigLevelTriggered :: Bool,
    --picConfigSlaveMask :: Uint8, -- use slave array
    --picConfigSlaveId :: Uint8,
    picConfigNested :: Bool,
    picConfigBuf :: Bool,
    --picConfigMode :: Master/Slave,
    picConfigAEOI :: Bool,
    picLastICW :: PicICW -- last ICW to expect
    --picConfigUPM :: Bool
} deriving (Show)


data PicState = PicState {
    picStateIMR :: Uint8,
    picStateIRR :: Uint8,
    picStateISR :: Uint8,
    picStateSMask :: Bool,
    picStateLowestPrio :: Uint8,
    picRotateOnAeoi :: Bool,
    picStatePolled :: Bool,
    picInitStage :: PicICW,
    picReadReg :: PicReadReg,
    picStateIntr :: Bool -- INTR sent to CPU
} deriving (Show)


data Pic = Pic {
    picConfig :: PicConfig,
    picState :: PicState
} deriving (Show)


{-data PicPC = PicPC {
    picPCMaster :: Pic,
    picPCSlave :: Pic
} deriving (Show)
-}

-------------------------------------------------------------------------------

defaultConfigPic = PicConfig False False 4 0 False False False False PicICW2
defaultStatePic = PicState 0 0 0 False 7 False False PicICW2 PicIRR False

isICW1 :: Uint8 -> Bool
isICW1 = (/= 0) . (.&. 0x10)

isOCW3 :: Uint8 -> Bool
isOCW3 = (== 0x08) . (.&. 0x18)

isSetPrio = (== 0xC0) . (.&. 0xF0)
isEIO = (== 0x60) . (.&. 0xF0)
isREIO = (== 0xE0) . (.&. 0xF0)

checkICWDone :: Pic -> Pic
checkICWDone (Pic config state) | (picInitStage state) == (picLastICW config) =
    (Pic config (state { picInitStage = PicICWDone } ))

picDecodeCommand :: Uint8 -> Pic -> [PicCommand]
picDecodeCommand val _ | isICW1(val) =
    [PicICWCommand PicICW1 val]
picDecodeCommand val _ | isOCW3(val) =
    if (val .&. 0x04) /= 0 then
        [PicPoll]
    else
        let read_v = val .&. 0x03
            mask_v = val .&. 0x60
            op = if read_v == 0x02
                    then [(PicRead PicIRR)]
                        else if read_v == 0x03
                            then [(PicRead PicISR)]
                                else []
            mask = if mask_v == 0x60
                      then [(PicSMCommand True)]
                          else if mask_v == 0x40 then
                              [(PicSMCommand False)]
                                else []
            in
        op ++ mask
picDecodeCommand val _ | val == 0x00 || val == 0x80 = [(PicRotateAEOI (val /= 0))]
picDecodeCommand val _ | val == 0x20 = [(PicEOI False Nothing)]
picDecodeCommand val _ | val == 0xA0 = [(PicEOI True Nothing)]
picDecodeCommand val _ | isSetPrio(val) = [(PicSetPrio val)]
picDecodeCommand val _ | isEIO(val) = [(PicEOI False (Just val))]
picDecodeCommand val _ | isREIO(val) = [(PicEOI True (Just val))]
picDecodeCommand _ _ = []

picDecodeData :: Uint8 -> PicICW -> [PicCommand]
picDecodeData val PicICWDone = [(PicSetIMR val)]
picDecodeData val icw = [(PicICWCommand icw val)]

picWriteControl :: Pic -> PicCommand -> (Pic, Bool)
picWriteControl _ (PicICWCommand PicICW1 val) =
    ((Pic config defaultStatePic), True)
    where
        config = defaultConfigPic { 
            picLastICW = if testBit val 0 then PicICW4 else PicICW2,
            picConfigSingle = testBit val 1,
            picConfigADI = if testBit val 2 then 4 else 8,
            picConfigLevelTriggered = testBit val 3
        }
picWriteControl (Pic config state) (PicICWCommand PicICW2 val) =
    (pic_, False)
    where
        config_ = config { picConfigAddr = (val .&. 0xF8) }
        pic_ = checkICWDone $ Pic config_ state
picWriteControl pic (PicICWCommand PicICW3 val) =
    (checkICWDone pic, False)
picWriteControl (Pic config state) (PicICWCommand PicICW4 val) =
    (pic_, True)
    where
        config_ = config { 
            picConfigAEOI = testBit val 1,
            picConfigBuf = False,
            picConfigNested = testBit val 4
        }
        pic_ = checkICWDone $ Pic config_ state
picWriteControl (Pic config state) PicPoll =
    ((Pic config state { picStatePolled = True }), False)
picWriteControl (Pic config state) (PicSMCommand val) =
    ((Pic config state { picStateSMask = val }), True)
picWriteControl (Pic config state) (PicRotateAEOI val) =
    ((Pic config state { picRotateOnAeoi = val }), False)
picWriteControl (Pic config state) (PicSetPrio val) =
    ((Pic config state { picStateLowestPrio = val}), True)
picWriteControl (Pic config state) (PicSetIMR val) =
    ((Pic config state { picStateIMR = val }), True)
picWriteControl (Pic config state) (PicRead reg) =
    ((Pic config state {picReadReg = reg}), False)
picWriteControl pic (PicEOI rotate Nothing) = -- non-specific EOI
    (pic, True)
picWriteControl pic (PicEOI rotate (Just level)) = -- specific EOI
    (pic, True)
picWriteControl pic _ = (pic, False)

picReadData :: Pic -> (Pic, Bool, Uint8)
picReadData (Pic config state) | picStatePolled state =
    --todo: eoi
    ((Pic config state { picStatePolled = False }), True, 0)
picReadData pic@(Pic _ state) =
    (pic, False, picStateIMR state)

picReadControl :: Pic -> (Pic, Bool, Uint8)
picReadControl (Pic config state) | picStatePolled state =
    ((Pic config state { picStatePolled = False }), True, 0)
picReadControl pic@(Pic _ state) =
    (pic, False, 
        if picReadReg state == PicIRR then picStateIRR state else picStateISR state)


getISRMask :: Uint8 -> Uint8 -> Uint8
getISRMask lowestPriority isr =
    complement $ getISRMask_ (setBit 0 (fromIntegral $ mod (lowestPriority + 1) 8)) 0 8
    where
        getISRMask_ :: Uint8 -> Uint8 -> Uint8 -> Uint8
        getISRMask_ _ val 0 = val
        getISRMask_ mask val n =
            getISRMask_ mask_ val_ (n - 1)
            where
                mask_ = rotateL mask 1
                val_ = if val /= 0 then val .|. mask else (val .|. (mask .&. isr))

picFindHighest :: Uint8 -> Uint8 -> Maybe Uint8
picFindHighest lowestPriority irr =
    findHighest $ mod (lwi + 1) 8
    where
        lwi = fromIntegral lowestPriority
        findHighest :: Int -> Maybe Uint8
        findHighest num | testBit irr num = Just $ fromIntegral num
        findHighest num | num == lwi = Nothing
        findHighest num = findHighest $ mod (num + 1) 8

picGetIntNum :: PicConfig -> Uint8 -> Uint8
picGetIntNum config irq = irq + picConfigAddr config

picUpdate :: Pic -> (Pic, PicAction)
picUpdate pic@(Pic config state) =
    case (picStateIntr state, intrActive) of
        (True, False) ->
            (Pic config state { picStateIntr = False }, PicClearInt)
        (False, True) ->
            (Pic config state { picStateIntr = True }, PicRaiseInt)
        _ -> (pic, PicNoAction)
    where
        intrActive = (maskedIRR .&. maskedISR) /= 0
        maskedISR = getISRMask (picStateLowestPrio state) (picStateISR state)
        maskedIRR = picStateIRR state .&. (complement $ picStateIMR state)

picAck :: Pic -> (Pic, Uint8)
picAck pic@(Pic config state) =
    case picFindHighest (picStateLowestPrio state) (picStateIRR state) of
        Just num -> 
            let picIRR_ = if picConfigLevel config then
                             picStateIRR state
                             else
                                clearBit (picStateIRR state) $ fromIntegral num
                picISR_ = if picConfigAEOI config then 
                             picStateISR state
                             else
                                 setBit (picStateISR state) $ fromIntegral num
                lowest_ = if picRotateOnAeoi state then num
                             else picStateLowestPrio state
                state_ = state {
                    picStateIRR = picIRR_,
                    picStateISR = picISR_,
                    picStateLowestPrio = lowest_
                }
                pic_ = Pic config state_
                in
            (pic_, picGetIntNum config num)
        Nothing ->
            (pic, picGetIntNum config 7)

-------------------------------------------------------------------------------
