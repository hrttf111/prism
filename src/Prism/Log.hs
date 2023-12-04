{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Prism.Log (
    commonF
-------------------------------------------------------------------------------
    , Cpu(..), Instruction(..), Jmp(..)
    , Pc, Bios, Video, Timer
    , logFeatureTree
    , showFeatures
    , (..>>), (..>)
-------------------------------------------------------------------------------
    , traceJmpIntra, traceJmpInter
    , traceCallNear, traceCallIntra, traceCallInter
    , traceRetIntra, traceRetInter
    , traceInterrupt, traceRetInterrupt
) where

import Data.Bits (shiftL)
import Numeric (showHex)

import Control.Monad.State.Strict

import Prism.Cpu

-------------------------------------------------------------------------------

commonF = LogFeature 0
showFeatures = putStrLn $ show logFeatureTree

-------------------------------------------------------------------------------

regsToOffset :: Uint16 -> Uint16 -> Int
regsToOffset csVal ipVal = ((shiftL (fromIntegral csVal) 4) + (fromIntegral ipVal)) :: Int

traceJmp :: (CpuMonad m) => Uint16 -> Uint16 -> Uint16 -> Uint16 -> String -> m ()
traceJmp ipValFrom csValFrom ipVal csVal msg =
    cpuLog Trace commonF $ msg ++ ": "
                             ++ "(ip=0x" ++ (showHex ipValFrom "")
                             ++ ";cs=0x" ++ (showHex csValFrom "")
                             ++ ";off=" ++ (show offsetFrom)
                             ++ ") -> (ip=0x" ++ (showHex ipVal "")
                             ++ ";cs=0x" ++ (showHex csVal "")
                             ++ ";off=" ++ (show offsetTo) ++ ")"
                             ++ "= " ++ (show offsetDiff)
    where
        offsetTo = regsToOffset csVal ipVal
        offsetFrom = regsToOffset csValFrom ipValFrom
        offsetDiff = offsetTo - offsetFrom

traceJmpIntra :: (CpuMonad m) => Uint16 -> m ()
traceJmpIntra ipVal =
    cpuDebugAction Trace commonF $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp ipValFrom csValFrom ipVal csValFrom "JMP[Intra]"

traceJmpInter :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceJmpInter ipVal csVal =
    cpuDebugAction Trace commonF $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp ipValFrom csValFrom ipVal csVal "JMP[Inter]"

traceCallNear :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceCallNear ipValFrom valPlus =
    cpuDebugAction Trace commonF $ do
        csValFrom <- readOp cs
        traceJmp ipValFrom csValFrom (ipValFrom + valPlus) csValFrom "Call[Near]"

traceCallIntra :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceCallIntra ipValFrom ipVal =
    cpuDebugAction Trace commonF $ do
        csValFrom <- readOp cs
        traceJmp ipValFrom csValFrom ipVal csValFrom "Call[Intra]"

traceCallInter :: (CpuMonad m) => Uint16 -> Uint16 -> Uint16 -> Uint16 -> m ()
traceCallInter ipValFrom csValFrom ipVal csVal =
    traceJmp ipValFrom csValFrom ipVal csVal "Call[Inter]"

traceRetIntra :: (CpuMonad m) => Uint16 -> m ()
traceRetIntra ipVal =
    cpuDebugAction Trace commonF $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp ipValFrom csValFrom ipVal csValFrom "Ret[Intra]"

traceRetInter :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceRetInter ipVal csVal =
    cpuDebugAction Trace commonF $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp ipValFrom csValFrom ipVal csVal "Ret[Inter]"

-------------------------------------------------------------------------------

traceInterrupt :: (CpuMonad m) => Uint8 -> m ()
traceInterrupt int =
    cpuLog Trace commonF $ "Interrupt = 0x" ++ showHex int ""

traceRetInterrupt :: (CpuMonad m) => m ()
traceRetInterrupt = cpuLog Trace commonF "Reti"

-------------------------------------------------------------------------------

class GetLogFeature container feature res | container feature -> res where
    getLogFeature :: container -> feature -> res

data LFeaturePair a b = LFeaturePair a b
data LFeatureLevel a b = LFeatureLevel a b LogFeature
data LFeatureTree a = LFeatureTree a

instance (Show a, Show b) => Show (LFeaturePair a b) where
    show (LFeaturePair a b) = "LFeaturePair(" ++ (show a) ++ ";" ++ (show b) ++ ")"

instance (Show a, Show b) => Show (LFeatureLevel a b) where
    show (LFeatureLevel a b n) = "LFeatureLevel(" ++ (show a) ++ ";" ++ (show b) ++ ";" ++ (show n) ++ ")"

instance (Show a) => Show (LFeatureTree a) where
    show (LFeatureTree a) = "LFeatureTree(" ++ (show a) ++ ")"

instance GetLogFeature (LFeatureLevel feature b) feature (LFeatureLevel feature b) where
    getLogFeature a@(LFeatureLevel _ b n) _ = a

instance {-# OVERLAPS #-} GetLogFeature (LFeaturePair (LFeatureLevel feature b') b) feature (LFeatureLevel feature b') where
    getLogFeature (LFeaturePair a _) f = getLogFeature a f

instance (GetLogFeature b feature res) => GetLogFeature (LFeaturePair a b) feature res where
    getLogFeature (LFeaturePair _ b) f = getLogFeature b f

class StartSearch a b | a -> b where
    startSearch :: a -> b

instance StartSearch (LFeatureLevel feature b) b where
    startSearch (LFeatureLevel _ b _) = b

instance StartSearch (LFeaturePair a b) (LFeaturePair a b) where
    startSearch = id

instance StartSearch (LFeatureTree a) a where
    startSearch (LFeatureTree a) = a

class GetLogFeatureVal a where
    getLogFeatureVal :: a -> LogFeature

instance GetLogFeatureVal (LFeatureLevel a b) where
    getLogFeatureVal (LFeatureLevel _ _ n) = n

infixl 6 ..>
(..>) :: (StartSearch container c1, GetLogFeature c1 feature res) => container -> feature -> res
container ..> feature = getLogFeature (startSearch container) feature

infixl 6 ..>>
(..>>) :: (GetLogFeatureVal res, StartSearch container c1, GetLogFeature c1 feature res) => container -> feature -> LogFeature
container ..>> feature = getLogFeatureVal $ getLogFeature (startSearch container) feature

--------------------------------------------------------------------------------

type BuilderState = State Int

infixr 6 .>>>
(.>>>) :: BuilderState (LFeaturePair a ()) -> BuilderState (LFeaturePair b k) -> BuilderState (LFeaturePair a (LFeaturePair b k))
am .>>> bm = do
    (LFeaturePair a ()) <- am
    b <- bm
    return $ LFeaturePair a b

mkLeaf :: feature -> BuilderState (LFeaturePair (LFeatureLevel feature ()) ())
mkLeaf feature = do
    n <- state (\n -> (n+1, n+1))
    return $ LFeaturePair (LFeatureLevel feature () (LogFeature n)) ()

mkLevel :: feature -> BuilderState b -> BuilderState (LFeaturePair (LFeatureLevel feature b) ())
mkLevel feature bm = do
    n <- state (\n -> (n+1, n+1))
    b <- bm
    return $ LFeaturePair (LFeatureLevel feature b (LogFeature n)) ()

data Cpu = Cpu deriving (Show)
data Instruction = Instruction deriving (Show)
data Jmp = Jmp deriving (Show)

data Pc = Pc deriving (Show)
data Bios = Bios deriving (Show)
data Video = Video deriving (Show)
data Timer = Timer deriving (Show)

logFeatureTree = LFeatureTree $ evalState (mkLevel Cpu (
                                               mkLevel Instruction (
                                                    mkLeaf Jmp
                                               )
                                          ) .>>>
                                          mkLevel Pc (
                                               mkLevel Bios (
                                                    mkLeaf Video .>>>
                                                    mkLeaf Timer
                                               )
                                          )) 0

--------------------------------------------------------------------------------
