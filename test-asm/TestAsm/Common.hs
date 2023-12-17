{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TestAsm.Common where

import Test.Hspec
import Test.Hspec.Expectations

import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

import Numeric (showHex)

import Data.Text (Text)
import qualified Data.ByteString as B

import Prism.Cpu
import Prism.Decoder (PrismDecoder)

import Assembler

-------------------------------------------------------------------------------

type PrismRunner = PrismDecoder -> Int -> PrismM ()

data TestEnv = TestEnv {
        peripheralThreadId :: Maybe ThreadId,
        assembleNative :: (Text -> IO B.ByteString),
        assembleNative16 :: (Text -> IO B.ByteString),
        executeNative :: (B.ByteString -> IO MemReg),
        executePrism :: (B.ByteString -> PrismRunner -> IO Ctx)
    }

data TestEnv1 executor = TestEnv1 {
        testEnv1Assemble :: (Text -> IO B.ByteString),
        testEnv1Executor :: executor
    }

data TestEnv2 executor1 executor2 = TestEnv2 {
        testEnv2Assemble1 :: (Text -> IO B.ByteString),
        testEnv2Executor1 :: executor1,
        testEnv2Assemble2 :: (Text -> IO B.ByteString),
        testEnv2Executor2 :: executor2
    }

-------------------------------------------------------------------------------

data MemRange = MemRange Uint32 Uint32 deriving (Eq)
newtype MemRangeRes = MemRangeRes [Uint8] deriving (Eq)

instance Show MemRange where
    show (MemRange start end) = "MemRange 0x" ++ (showHex start "") ++ " 0x" ++ (showHex end "")

instance Show MemRangeRes where
    show (MemRangeRes range) =
        foldl printHex "" range
        where
            printHex s b = "0x" ++ (showHex b "") ++ ", " ++ s

data AllRegs = AllRegs deriving (Show, Eq)

-------------------------------------------------------------------------------

class (MemRegManipulator a MemReg v) => RegTest a v | a -> v where
    shouldEq :: (HasCallStack) => a -> Int -> MemReg -> Expectation
    shouldEqReg :: (HasCallStack) => a -> MemReg -> MemReg -> Expectation

instance RegTest Reg8 Uint8 where
    shouldEq reg valExp memReg = do
        val <- readRegRaw memReg reg
        val `shouldBe` (fromIntegral valExp)
    shouldEqReg reg memReg1 memReg2 = do
        val1 <- readRegRaw memReg1 reg
        val2 <- readRegRaw memReg2 reg
        val1 `shouldBe` val2

instance RegTest Reg16 Uint16 where
    shouldEq reg valExp memReg = do
        val <- readRegRaw memReg reg
        val `shouldBe` (fromIntegral valExp)
    shouldEqReg reg memReg1 memReg2 = do
        val1 <- readRegRaw memReg1 reg
        val2 <- readRegRaw memReg2 reg
        val1 `shouldBe` val2

flagsShouldEq :: (HasCallStack) => Flags -> MemReg -> Expectation
flagsShouldEq flags memReg = do
    (flagsN, _) <- readRegRaw memReg flagsInternal
    flags `shouldBe` flagsN

-------------------------------------------------------------------------------

class (Monad m) => HasSourceL s m | m -> s where
    getSourceL :: m s

class (Monad m) => HasSourceR s m | m -> s where
    getSourceR :: m s

class (Monad m) => OperandSupport source oper val m | oper -> val where
    readSourceOp :: source -> oper -> m val

showAllRegsL :: (HasCallStack, HasSourceL sl m, OperandSupport sl AllRegs String m, MonadIO m) => m ()
showAllRegsL = do
    sourceL <- getSourceL
    val <- readSourceOp sourceL AllRegs
    liftIO $ putStrLn val

showAllRegsR :: (HasCallStack, HasSourceR sr m, OperandSupport sr AllRegs String m, MonadIO m) => m ()
showAllRegsR = do
    sourceR <- getSourceR
    val <- readSourceOp sourceR AllRegs
    liftIO $ putStrLn val

showAllRegs :: (HasCallStack, HasSourceL sl m, OperandSupport sl AllRegs String m, MonadIO m) => m ()
showAllRegs = showAllRegsL

showOperandVal :: (HasCallStack, Show op, Show val, Eq val, HasSourceL sl m, OperandSupport sl op val m, MonadIO m) => op -> m ()
showOperandVal op = do
    sourceL <- getSourceL
    valL <- readSourceOp sourceL op
    liftIO $ putStrLn $ (show op) ++ " = " ++ (show valL)

cmpOperandVal :: (HasCallStack, Show val, Eq val, HasSourceL sl m, OperandSupport sl op val m, MonadIO m) => op -> val -> m ()
cmpOperandVal op val = do
    sourceL <- getSourceL
    valL <- readSourceOp sourceL op
    liftIO $ valL `shouldBe` val

cmpOperandsVals :: (HasCallStack, Show val, Eq val, HasSourceL sl m, OperandSupport sl op val m, MonadIO m) => [op] -> [val] -> m ()
cmpOperandsVals ops vals = do
    when ((length ops) /= (length vals)) $
        liftIO $ expectationFailure $ "OPs len != VALs len: " ++ (show $ length ops) ++ " != " ++ (show $ length vals)
    mapM_ (\(op, val) -> cmpOperandVal op val) $ zip ops vals

cmpOperandSources :: (HasCallStack, Show val, Eq val, HasSourceL sl m, HasSourceR sr m, OperandSupport sl op val m, OperandSupport sr op val m, MonadIO m) => op -> m ()
cmpOperandSources op = do
    sourceL <- getSourceL
    sourceR <- getSourceR
    valL <- readSourceOp sourceL op
    valR <- readSourceOp sourceR op
    liftIO $ valL `shouldBe` valR

cmpOperandsSources :: (HasCallStack, Show val, Eq val, HasSourceL sl m, HasSourceR sr m, OperandSupport sl op val m, OperandSupport sr op val m, MonadIO m) => [op] -> m ()
cmpOperandsSources ops =
    mapM_ cmpOperandSources ops

-------------------------------------------------------------------------------

class ProgramExecutor exec res m | exec m -> res where
    execProgram :: exec -> B.ByteString -> m res

-------------------------------------------------------------------------------

newtype SeqTransM s m a = SeqTransM {
    runSeqTM  :: (StateT s m) a
} deriving (Monad, Applicative, Functor, MonadState s)

instance MonadTrans (SeqTransM s) where
    lift = SeqTransM . lift

instance MonadIO m => MonadIO (SeqTransM s m) where
    liftIO = lift . liftIO

type SeqM s = SeqTransM s IO

instance (s ~ (s1, s2)) => HasSourceL s1 (SeqM s) where
    getSourceL = fst <$> get

instance (s ~ (s1, s2)) => HasSourceR s2 (SeqM s) where
    getSourceR = snd <$> get

runSeq :: (HasCallStack) => s -> SeqM s () -> IO s
runSeq s m = snd <$> (runStateT $ runSeqTM m) s

-------------------------------------------------------------------------------
