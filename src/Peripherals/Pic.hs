{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Peripherals.Pic where

import Control.Monad.Trans

import Data.Bits

import Prism
import PrismPeripheral

-------------------------------------------------------------------------------

data PicReadReg = PicIRR | PicISR deriving (Show)
data PicICW = PicICW1
              | PicICW2
              | PicICW3
              | PicICW4
              | PicICWDone
              deriving (Show)

data PicAction = PicClearInt | PicRaiseInt deriving (Show)
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

defaultConfigPic = PicConfig False False 4 0 False False False PicICW2
defaultStatePic = PicState 0 0 0 False 0 False False PicICW2 PicIRR

isICW1 :: Uint8 -> Bool
isICW1 = (/= 0) . (.&. 0x10)

isOCW3 :: Uint8 -> Bool
isOCW3 = (== 0x08) . (.&. 0x18)

isSetPrio = (== 0xC0) . (.&. 0xF0)
isEIO = (== 0x60) . (.&. 0xF0)
isREIO = (== 0xE0) . (.&. 0xF0)

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

--picWriteControl :: PicCommand -> Pic -> (Pic, PicAction)
