{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GDB.Protocol where

import Data.Bits (shiftR, shiftL, (.&.), (.|.), FiniteBits, finiteBitSize, countLeadingZeros)
import Data.Word
import Numeric

import Text.Parsec
import Text.Parsec.Text
import Text.Read (readMaybe)
import qualified Data.Text as T

-------------------------------------------------------------------------------

data GDBQuery = GQSupported 
                | GQSThreadInfo
                | GQFThreadInfo
                | GQAttached
                | GQTStatus
                | GQCurrentThread
                | GQUknownQuery deriving (Show, Eq)

data GDBPacket = GHaltReason
                 | GCont { addr :: Maybe Int }
                 | GDetach
                 | GKill
                 | GReadGRegs
                 | GWriteGReg
                 | GStep
                 | GReadMem (Int, Int)
                 | GWriteMem (Int, [Word8])
                 | GReadReg Int
                 | GWriteReg (Int, Int)
                 | GQuery GDBQuery
                 | GQuerySet
                 | GMustReplyEmpty
                 | GThreadOp
                 | GThreadSetOp
                 | GBreakPoint (Int, Int)
                 | GBreakPointRemove (Int, Int)
                 | GUknownCommand deriving (Show, Eq)

-------------------------------------------------------------------------------

assembleVal :: (FiniteBits a, Integral a, Show a) => [Word8] -> Int -> a -> a
assembleVal [] _ val = val
assembleVal (byte:tail) pos val = assembleVal tail (pos + 8) $ val + (shiftL (fromIntegral byte) pos)

assembleHex :: (FiniteBits a, Integral a, Show a) => a -> Int -> String
assembleHex val shiftV | (finiteBitSize val) <= shiftV = ""
assembleHex val shiftV = (if byte > 0x0F then showHex byte else ('0':) . showHex byte) $ assembleHex (shiftR val 8) (shiftV + 8)
    where
        byte = val .&. 0xFF

assembleHex1 :: (FiniteBits a, Integral a, Show a) => a -> String -> Int -> String
assembleHex1 val s shiftV | (finiteBitSize val) <= shiftV = s
assembleHex1 val s shiftV = (if byte > 0x0F then showHex byte else ('0':) . showHex byte) $ assembleHex1 (shiftR val 8) s (shiftV + 8)
    where
        byte = val .&. 0xFF

toHex :: (FiniteBits a, Integral a, Show a) => a -> String
toHex val = assembleHex val 0

toHex1 :: (FiniteBits a, Integral a, Show a) => a -> String -> String
toHex1 val s = assembleHex1 val s 0

fromHex :: (FiniteBits a, Integral a, Show a) => [Word8] -> a
fromHex bytes = assembleVal bytes 0 0

-------------------------------------------------------------------------------

type GDBParser = Parsec T.Text ()

decParser :: GDBParser Int
decParser = read <$> many1 digit

decParserTill :: Char -> GDBParser Int
decParserTill c = read <$> manyTill digit (try $ char c)

hexParser :: GDBParser Int
hexParser = fst . head . readHex <$> many1 hexDigit

hexByteParser :: GDBParser Word8
hexByteParser = fst . head . readHex <$> count 2 hexDigit

hexWordParser :: (FiniteBits a, Integral a, Show a) => GDBParser a
hexWordParser = fromHex <$> many hexByteParser

addrLenParser :: GDBParser (Int, Int)
addrLenParser = (,) <$> hexParser  <*> (char ',' >> decParser)

addrParser :: GDBParser (Maybe Int)
addrParser = (>>= readMaybe) <$> optionMaybe (many1 digit)

regParser :: GDBParser (Int, Int)
regParser = (,) <$> hexParser <*> (char '=' >> hexWordParser)

breakParser :: GDBParser (Int, Int)
breakParser = (,) <$> (char ',' >> hexParser) <*> (char ',' >> decParser)

memParse :: GDBParser (Int, [Word8])
memParse = (,) <$> hexParser <*> (char ',' >> decParserTill ':' >>= flip count hexByteParser)

gdbQueryParser :: GDBParser GDBPacket
gdbQueryParser = choice [
                   string "Supported" >> return (GQuery GQSupported)
                 , string "fThreadInfo" >> return (GQuery GQFThreadInfo)
                 , string "sThreadInfo" >> return (GQuery GQSThreadInfo)
                 , string "Attached" >> return (GQuery GQAttached)
                 , string "TStatus" >> return (GQuery GQTStatus)
                 , char 'C' >> return (GQuery GQCurrentThread)
                 , anyToken >> return (GQuery GQUknownQuery)
                 ]

gdbParser :: GDBParser GDBPacket 
gdbParser = choice [
              char '?' >> return GHaltReason
            , GCont <$> (char 'c' >> addrParser)
            , char 'D' >> return GDetach
            , char 'g' >> return GReadGRegs
            , GReadReg <$> (char 'p' >> hexParser)
            , GWriteReg <$> (char 'P' >> regParser)
            , GReadMem <$> (char 'm' >> addrLenParser)
            , GWriteMem <$> (char 'M' >> memParse)
            , char 's' >> return GStep
            , char 'q' >> gdbQueryParser
            , char 'v' >> choice [
                string "MustReplyEmpty" >> return GMustReplyEmpty
                , string "Kill" >> return GKill
            ]
            , char 'H' >> return GThreadSetOp
            , char 'T' >> return GThreadOp
            , GWriteMem <$> (char 'X' >> memParse)
            , GBreakPoint <$> (string "Z0" >> breakParser)
            , GBreakPointRemove <$> (string "z0" >> breakParser)
            , anyToken >> return GUknownCommand
            ]

-------------------------------------------------------------------------------
