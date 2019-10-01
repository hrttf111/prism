{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prism where

import Data.Word (Word8, Word16)

--import qualified Data.ByteString as BS

type Uint8 = Word8
type Uint16 = Word16
-- Note: Max size of instruction is 6 bytes
type InstrBytes = (Uint8, Uint8, Uint8, Uint8, Uint8, Uint8)

data Ctx = Ctx String deriving (Show)
