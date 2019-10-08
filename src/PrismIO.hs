{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PrismIO where

import Foreign.Ptr
--import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.Marshal.Array (pokeArray)

import Data.Word (Word8)
import qualified Data.ByteString as BS

import Control.Monad.Trans (liftIO, MonadIO)
import System.IO (FilePath)

readCodeToPtr :: MonadIO m => FilePath -> Ptr Word8 -> Int -> m (Ptr Word8)
readCodeToPtr filePath ptr offset = liftIO $ do
    bs <- BS.readFile filePath
    let array = BS.unpack bs
        ptrN = plusPtr ptr offset
    pokeArray ptrN array
    return ptr
