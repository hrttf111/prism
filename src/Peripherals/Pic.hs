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
    picReadReg :: PicReadReg
} deriving (Show)


data Pic = Pic {
    picConfig :: PicConfig,
    picState :: PicState
} deriving (Show)

-------------------------------------------------------------------------------

defaultConfigPic = PicConfig False False 4 0 False False False False PicICW2
defaultStatePic = PicState 0 0 0 False 0 False False PicICW2 PicIRR

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

picWriteControl :: Pic -> PicCommand -> (Pic, PicAction)
picWriteControl _ (PicICWCommand PicICW1 val) =
    ((Pic config defaultStatePic), PicClearInt)
    where
        config = defaultConfigPic { 
            picLastICW = if testBit val 0 then PicICW4 else PicICW2,
            picConfigSingle = testBit val 1,
            picConfigADI = if testBit val 2 then 4 else 8,
            picConfigLevelTriggered = testBit val 3
        }
picWriteControl (Pic config state) (PicICWCommand PicICW2 val) =
    (pic_, PicNoAction)
    where
        config_ = config { picConfigAddr = (val .&. 0xF8) }
        pic_ = checkICWDone $ Pic config_ state
picWriteControl pic (PicICWCommand PicICW3 val) =
    (checkICWDone pic, PicNoAction)
picWriteControl (Pic config state) (PicICWCommand PicICW4 val) =
    (pic_, PicClearInt)
    where
        config_ = config { 
            picConfigAEOI = testBit val 1,
            picConfigBuf = False,
            picConfigNested = testBit val 4
        }
        pic_ = checkICWDone $ Pic config_ state
picWriteControl (Pic config state) PicPoll =
    ((Pic config state { picStatePolled = True }), PicNoAction)
picWriteControl (Pic config state) (PicSMCommand val) =
    ((Pic config state { picStateSMask = val }), PicNoAction)
picWriteControl (Pic config state) (PicRotateAEOI val) =
    ((Pic config state { picRotateOnAeoi = val }), PicNoAction)
picWriteControl (Pic config state) (PicSetPrio val) =
    ((Pic config state { picStateLowestPrio = val}), PicNoAction)
picWriteControl (Pic config state) (PicSetIMR val) =
    ((Pic config state { picStateIMR = val }), PicNoAction)
picWriteControl (Pic config state) (PicRead reg) =
    ((Pic config state {picReadReg = reg}), PicNoAction)
picWriteControl pic (PicEOI rotate Nothing) = -- non-specific EOI
    (pic, PicNoAction)
picWriteControl pic (PicEOI rotate (Just level)) = -- specific EOI
    (pic, PicNoAction)
picWriteControl pic _ = (pic, PicNoAction)

picReadData :: Pic -> (Pic, PicAction, Uint8)
picReadData (Pic config state) | picStatePolled state =
    ((Pic config state { picStatePolled = False }), PicNoAction, 0)
picReadData pic@(Pic _ state) =
    (pic, PicNoAction, picStateIMR state)

picReadControl :: Pic -> (Pic, PicAction, Uint8)
picReadControl (Pic config state) | picStatePolled state =
    ((Pic config state { picStatePolled = False }), PicNoAction, 0)
picReadControl pic@(Pic _ state) =
    (pic, PicNoAction, 
        if picReadReg state == PicIRR then picStateIRR state else picStateISR state)
