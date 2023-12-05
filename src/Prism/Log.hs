{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TypeFamilies #-}

module Prism.Log (
    cpuLogT, cpuDebugActionT
-------------------------------------------------------------------------------
    , CpuJmp(..), CpuInt(..)
    , BiosGeneric(..), BiosVideo(..), BiosTimer(..), BiosDisk(..), BiosKeyboard(..)
-------------------------------------------------------------------------------
    , logFeatureTree, logFeatureCount
    , showFeatures
    , featureArray
    , setFeatureLevel, (.=)
-------------------------------------------------------------------------------
    , traceJmpIntra, traceJmpInter
    , traceCallNear, traceCallIntra, traceCallInter
    , traceRetIntra, traceRetInter
    , traceInterrupt, traceRetInterrupt
) where

import Data.Array (Array, array, accumArray, (!), bounds, (//))
import Data.Typeable (typeOf)
import Data.Bits (shiftL)
import Numeric (showHex)

import Control.Monad.State.Strict

import Prism.Cpu

-------------------------------------------------------------------------------

showFeatures = show logFeatureTree

-------------------------------------------------------------------------------

--cpuLogT :: (CpuMonad m, GetLogFeatureVal res, GetLogFeature c1 feature res) => level -> feature -> String -> m ()
--cpuLogT :: (CpuMonad m, CpuDebugM m level) => level -> feature -> String -> m ()
--cpuLogT :: (CpuMonad m, CpuDebugM m level, GetLogFeatureVal res, GetLogFeature c1 feature res) => level -> feature -> String -> m ()
--cpuLogT :: (c1 ~ typeOf logFeatureTree, CpuMonad m, CpuDebugM m level, GetLogFeatureVal res, GetLogFeature c1 feature res) => level -> feature -> String -> m ()
cpuLogT level feature str = cpuLog level featureId str
    where
        featureId = logFeatureTree ..>> feature

cpuDebugActionT level feature m = cpuDebugAction level featureId m
    where
        featureId = logFeatureTree ..>> feature

-------------------------------------------------------------------------------

regsToOffset :: Uint16 -> Uint16 -> Int
regsToOffset csVal ipVal = ((shiftL (fromIntegral csVal) 4) + (fromIntegral ipVal)) :: Int

traceJmp :: (CpuMonad m) => Uint16 -> Uint16 -> Uint16 -> Uint16 -> String -> m ()
traceJmp ipValFrom csValFrom ipVal csVal msg =
    cpuLogT Trace CpuJmp $ msg ++ ": "
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
    cpuDebugActionT Trace CpuJmp $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp ipValFrom csValFrom ipVal csValFrom "JMP[Intra]"

traceJmpInter :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceJmpInter ipVal csVal =
    cpuDebugActionT Trace CpuJmp $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp ipValFrom csValFrom ipVal csVal "JMP[Inter]"

traceCallNear :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceCallNear ipValFrom valPlus =
    cpuDebugActionT Trace CpuJmp $ do
        csValFrom <- readOp cs
        traceJmp ipValFrom csValFrom (ipValFrom + valPlus) csValFrom "Call[Near]"

traceCallIntra :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceCallIntra ipValFrom ipVal =
    cpuDebugActionT Trace CpuJmp $ do
        csValFrom <- readOp cs
        traceJmp ipValFrom csValFrom ipVal csValFrom "Call[Intra]"

traceCallInter :: (CpuMonad m) => Uint16 -> Uint16 -> Uint16 -> Uint16 -> m ()
traceCallInter ipValFrom csValFrom ipVal csVal =
    traceJmp ipValFrom csValFrom ipVal csVal "Call[Inter]"

traceRetIntra :: (CpuMonad m) => Uint16 -> m ()
traceRetIntra ipVal =
    cpuDebugActionT Trace CpuJmp $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp ipValFrom csValFrom ipVal csValFrom "Ret[Intra]"

traceRetInter :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceRetInter ipVal csVal =
    cpuDebugActionT Trace CpuJmp $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp ipValFrom csValFrom ipVal csVal "Ret[Inter]"

-------------------------------------------------------------------------------

traceInterrupt :: (CpuMonad m) => Uint8 -> m ()
traceInterrupt int =
    cpuLogT Trace CpuInt $ "Interrupt = 0x" ++ showHex int ""

traceRetInterrupt :: (CpuMonad m) => m ()
traceRetInterrupt = cpuLogT Trace CpuInt "Reti"

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

data CpuJmp = CpuJmp deriving (Show)
data CpuInt = CpuInt deriving (Show)

data BiosGeneric = BiosGeneric deriving (Show)
data BiosVideo = BiosVideo deriving (Show)
data BiosTimer = BiosTimer deriving (Show)
data BiosDisk = BiosDisk deriving (Show)
data BiosKeyboard = BiosKeyboard deriving (Show)

logFeatures = runState (
                        mkLeaf CpuJmp .>>>
                        mkLeaf CpuInt .>>>
                        mkLeaf BiosGeneric .>>>
                        mkLeaf BiosVideo .>>>
                        mkLeaf BiosTimer .>>>
                        mkLeaf BiosDisk .>>>
                        mkLeaf BiosKeyboard
                       ) 0

logFeatureTree = fst logFeatures
logFeatureCount = snd logFeatures

featureArray :: Array Int Int
featureArray = newArray $ fromEnum Error

newArray :: Int -> Array Int Int
newArray level = array (1, logFeatureCount) [(i, level) | i <- [1..logFeatureCount]]

setFeatureLevel feature level ar = ar // [(featureIndex, levelIndex)]
    where
        levelIndex = fromEnum level
        (LogFeature featureIndex) = logFeatureTree ..>> feature

feature .= level = \ ar -> setFeatureLevel feature level ar

--------------------------------------------------------------------------------
