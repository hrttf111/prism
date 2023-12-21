{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE OverloadedStrings #-}

module TestAsm.Common where

import Test.Hspec
import Test.Hspec.Expectations

import Control.Concurrent (ThreadId, MVar, killThread, tryTakeMVar, threadDelay)
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.State.Strict

import Numeric (showHex)

import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.ByteString as B

import Prism.Cpu
import Prism.Decoder (PrismDecoder)

import Assembler

-------------------------------------------------------------------------------

data PeripheralThread = PeripheralThread {
    peripheralThreadId1 :: ThreadId,
    peripheralThreadMVar :: MVar ()
}

data TestEnv1 executor = TestEnv1 {
        testEnv1PeripheralThread :: Maybe PeripheralThread,
        testEnv1Assemble :: (Text -> IO B.ByteString),
        testEnv1Executor :: executor
    }

data TestEnv2 executor1 executor2 = TestEnv2 {
        testEnv2Assemble1 :: (Text -> IO B.ByteString),
        testEnv2Executor1 :: executor1,
        testEnv2Assemble2 :: (Text -> IO B.ByteString),
        testEnv2Executor2 :: executor2
    }

class TestEnvE env s m | env -> s where
    execTestEnv :: env -> Text -> SeqM s () -> m ()

instance (ProgramExecutor exec res IO) => TestEnvE (TestEnv1 exec) (res, ()) IO where
    execTestEnv env program seq = do
        code <- (testEnv1Assemble env) program
        res <- execProgram (testEnv1Executor env) code
        waitThreadEnd $ testEnv1PeripheralThread env
        runSeq (res, ()) seq
        return ()
        where
            waitN threadId mvar 0 = do
                killThread threadId
                putStrLn "Thread wait timeout" >> return ()
            waitN threadId mvar n = do
                m <- tryTakeMVar mvar
                if isJust m then
                    return ()
                    else do
                        threadDelay 10000
                        waitN threadId mvar $ n - 1
            waitThreadEnd (Just (PeripheralThread id mvar)) =
                waitN id mvar 10
            waitThreadEnd Nothing = return ()

instance (ProgramExecutor exec1 res1 IO, ProgramExecutor exec2 res2 IO) => TestEnvE (TestEnv2 exec1 exec2) (res1, res2) IO where
    execTestEnv env program seq = do
        code1 <- (testEnv2Assemble1 env) program
        res1 <- execProgram (testEnv2Executor1 env) code1
        code2 <- (testEnv2Assemble2 env) program
        res2 <- execProgram (testEnv2Executor2 env) code2
        runSeq (res1, res2) seq
        return ()

class TestEnvMaker maker env | maker -> env where
    makeTestEnv :: maker -> IO env

runTest maker program seq = do
    env <- makeTestEnv maker
    execTestEnv env program seq

execTestEnvIO :: (ProgramExecutor exec res IO) => (TestEnv1 exec) -> Text -> SeqM (res, ()) () -> IO ()
execTestEnvIO env program seq = execTestEnv env program seq

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

type family OpVal o where
    OpVal Reg8 = Uint8
    OpVal Reg16 = Uint16
    OpVal RegSeg = Uint16
    OpVal Flag = Bool
    OpVal EFlag = Bool
    OpVal MemPhy8 = Uint8
    OpVal MemPhy16 = Uint16

class ShouldEq op val m where
    shouldEq1 :: (HasCallStack) => op -> val -> m ()

instance {-# OVERLAPS #-} (Show val, Eq val, HasSourceL sl m, OperandSupport sl op (OpVal op) m, val ~ (OpVal op), MonadIO m) => ShouldEq [op] [val] m where
    shouldEq1 = cmpOperandsVals

instance {-# OVERLAPPABLE #-} (Show val, Eq val, HasSourceL sl m, OperandSupport sl op (OpVal op) m, val ~ (OpVal op), MonadIO m) => ShouldEq op val m where
    shouldEq1 = cmpOperandVal

class ShouldEqSources op m where
    shouldEqSources :: (HasCallStack) => op -> m ()

instance {-# OVERLAPS #-} (HasSourceL sl m, Show (OpVal op), Eq (OpVal op), HasSourceR sr m, OperandSupport sl op (OpVal op) m, OperandSupport sr op (OpVal op) m, MonadIO m) => ShouldEqSources [op] m where
    shouldEqSources ops = cmpOperandsSources ops

instance {-# OVERLAPPABLE #-} (HasSourceL sl m, Show (OpVal op), Eq (OpVal op), HasSourceR sr m, OperandSupport sl op (OpVal op) m, OperandSupport sr op (OpVal op) m, MonadIO m) => ShouldEqSources op m where
    shouldEqSources op = cmpOperandSources op

shouldEqSourcesAllFlags :: (ShouldEqSources [Flag] m) => m ()
shouldEqSourcesAllFlags = shouldEqSources [CF, PF, AF, ZF, SF, OF]

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
