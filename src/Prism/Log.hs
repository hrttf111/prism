{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Prism.Log (
    cpuLogT, cpuDebugActionT
    , cpuLogIP
-------------------------------------------------------------------------------
    , PrismCommon(..), PrismCommand(..), PrismRun(..), PrismPc(..)
    , CpuHalt(..)
    , CpuJmpInter(..), CpuJmpIntra(..), CpuCallInter(..), CpuCallIntra(..), CpuInt(..)
    , CpuStrings(..)
    , BiosGeneric(..), BiosVideo(..), BiosTimer(..), BiosDisk(..), BiosKeyboard(..)
-------------------------------------------------------------------------------
    , logFeatureTree, logFeatureCount
    , showFeatures
    , featureArray, makeFeatureArray
    , getFeatureId
    , setFeatureLevel, (.=), (..=), (..>>)
    , intToLevelName
    , intToFeatureName
-------------------------------------------------------------------------------
    , traceJmpIntra, traceJmpInter
    , traceCallNear, traceCallIntra, traceCallInter
    , traceRetIntra, traceRetInter
    , traceInterrupt, traceRetInterrupt
    , traceLogString
) where

import Data.Array (Array, array, (//), (!))
import Data.Typeable (typeOf)
import Data.Bits (shiftL)
import Numeric (showHex)

import Control.Monad.State.Strict

import Prism.Cpu

-------------------------------------------------------------------------------

showFeatures = show logFeatureTree

getFeatureId feature = fid
    where
        LogFeature fid = logFeatureTree ..>> feature

-------------------------------------------------------------------------------

cpuLogT level feature str = cpuLog level featureId str
    where
        featureId = logFeatureTree ..>> feature

cpuDebugActionT level feature m = cpuDebugAction level featureId m
    where
        featureId = logFeatureTree ..>> feature

-------------------------------------------------------------------------------

regsToOffset :: Uint16 -> Uint16 -> Int
regsToOffset csVal ipVal = ((shiftL (fromIntegral csVal) 4) + (fromIntegral ipVal)) :: Int

traceJmp feature ipValFrom csValFrom ipVal csVal msg =
    cpuLogT Trace feature $ msg ++ ": "
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
    cpuDebugActionT Trace CpuJmpIntra $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp CpuJmpIntra ipValFrom csValFrom ipVal csValFrom "JMP[Intra]"

traceJmpInter :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceJmpInter ipVal csVal =
    cpuDebugActionT Trace CpuJmpInter $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp CpuJmpInter ipValFrom csValFrom ipVal csVal "JMP[Inter]"

traceCallNear :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceCallNear ipValFrom valPlus =
    cpuDebugActionT Trace CpuJmpIntra $ do
        csValFrom <- readOp cs
        traceJmp CpuJmpIntra ipValFrom csValFrom (ipValFrom + valPlus) csValFrom "Call[Near]"

traceCallIntra :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceCallIntra ipValFrom ipVal =
    cpuDebugActionT Trace CpuCallIntra $ do
        csValFrom <- readOp cs
        traceJmp CpuCallIntra ipValFrom csValFrom ipVal csValFrom "Call[Intra]"

traceCallInter :: (CpuMonad m) => Uint16 -> Uint16 -> Uint16 -> Uint16 -> m ()
traceCallInter ipValFrom csValFrom ipVal csVal =
    traceJmp CpuCallInter ipValFrom csValFrom ipVal csVal "Call[Inter]"

traceRetIntra :: (CpuMonad m) => Uint16 -> m ()
traceRetIntra ipVal =
    cpuDebugActionT Trace CpuCallIntra $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp CpuCallIntra ipValFrom csValFrom ipVal csValFrom "Ret[Intra]"

traceRetInter :: (CpuMonad m) => Uint16 -> Uint16 -> m ()
traceRetInter ipVal csVal =
    cpuDebugActionT Trace CpuCallInter $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        traceJmp CpuCallInter ipValFrom csValFrom ipVal csVal "Ret[Inter]"

-------------------------------------------------------------------------------

traceLogString :: (CpuMonad m, Show n, Show n1, Integral n, Integral n1) => n -> n -> n1 -> n1 -> String -> m ()
traceLogString siVal diVal regVal regVal2 msg = do
    cpuDebugActionT Trace CpuStrings $ do
        csVal <- readOp cs
        ipVal <- readOp ip
        cxVal <- readOp cx
        let offset = regsToOffset csVal ipVal
        cpuLogT Trace CpuStrings $ "0x" ++ (showHex offset "")
                                   ++ "(cs=0x" ++ (showHex csVal "")
                                   ++ ",ip=0x" ++ (showHex ipVal "")
                                   ++ "): si=0x" ++ (showHex siVal "")
                                   ++ ",di=0x" ++ (showHex diVal "")
                                   ++ ",reg1=0x" ++ (showHex regVal $ "/" ++ (show (toEnum $ fromEnum regVal :: Char)))
                                   ++ ",reg2=0x" ++ (showHex regVal2 $ "/" ++ (show (toEnum $ fromEnum regVal2 :: Char)))
                                   ++ ",cx=" ++ (show cxVal)
                                   ++ " " ++ msg

cpuLogIP level feature msg = do
    cpuDebugActionT Trace feature $ do
        csVal <- readOp cs
        ipVal <- readOp ip
        let offset = regsToOffset csVal ipVal
        cpuLogT level feature $ "0x" ++ (showHex offset "")
                                ++ "(cs=0x" ++ (showHex csVal "")
                                ++ ",ip=0x" ++ (showHex ipVal "")
                                ++ "): " ++ msg

-------------------------------------------------------------------------------

traceInterrupt :: (CpuMonad m) => Uint8 -> m ()
traceInterrupt int =
    cpuDebugActionT Trace CpuInt $ do
        csValFrom <- readOp cs
        ipValFrom <- readOp ip
        let offset = 4 * (fromIntegral int)
            mem = MemPhy16 offset
            mem2 = MemPhy16 $ offset + 2
        ipVal <- readOp mem
        csVal <- readOp mem2
        traceJmp CpuInt ipValFrom csValFrom ipVal csVal $ "Interrupt = 0x" ++ showHex int ""
        when (int == 0x21) $ do -- BIOS interrupts
            ahVal <- readOp ah
            cpuLogT Trace CpuInt $ "AH = 0x" ++ showHex ahVal ""
        return ()

traceRetInterrupt :: (CpuMonad m) => Uint16 -> Uint16 -> Uint16 -> Uint16 -> m ()
traceRetInterrupt csValFrom ipValFrom csVal ipVal =
    traceJmp CpuInt ipValFrom csValFrom ipVal csVal "Reti"

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

data PrismCommon = PrismCommon deriving (Show)
data PrismCommand = PrismCommand deriving (Show)
data PrismRun = PrismRun deriving (Show)
data PrismPc = PrismPc deriving (Show)

data CpuHalt = CpuHalt deriving (Show)
data CpuJmpInter = CpuJmpInter deriving (Show)
data CpuJmpIntra = CpuJmpIntra deriving (Show)
data CpuCallInter = CpuCallInter deriving (Show)
data CpuCallIntra = CpuCallIntra deriving (Show)
data CpuInt = CpuInt deriving (Show)
data CpuStrings = CpuStrings deriving (Show)

data BiosGeneric = BiosGeneric deriving (Show)
data BiosVideo = BiosVideo deriving (Show)
data BiosTimer = BiosTimer deriving (Show)
data BiosDisk = BiosDisk deriving (Show)
data BiosKeyboard = BiosKeyboard deriving (Show)

logFeatures = runState (
                        mkLeaf PrismCommon .>>>
                        mkLeaf PrismCommand .>>>
                        mkLeaf PrismRun .>>>
                        mkLeaf PrismPc .>>>
                        mkLeaf CpuHalt .>>>
                        mkLeaf CpuJmpInter .>>>
                        mkLeaf CpuJmpIntra .>>>
                        mkLeaf CpuInt .>>>
                        mkLeaf CpuCallInter .>>>
                        mkLeaf CpuCallIntra .>>>
                        mkLeaf CpuStrings .>>>
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

data LogConfigAction = LogConfigAction {
    logConfigAction :: Array Int Int -> Array Int Int
}

instance Semigroup LogConfigAction where
    (LogConfigAction a1) <> (LogConfigAction a2) = LogConfigAction (\ar -> a2 $ a1 ar)

feature .= level = LogConfigAction $ setFeatureLevel feature level

makeFeatureArray :: LogConfigAction -> Array Int Int
makeFeatureArray (LogConfigAction a) = a featureArray

feature ..= level = \ ar -> setFeatureLevel feature level ar

--------------------------------------------------------------------------------

class IterLogFeature container where
    iterLogFeature :: container -> a -> (a -> (String, LogFeature) -> a) -> a

instance (Show a) => IterLogFeature (LFeatureLevel a b) where
    iterLogFeature (LFeatureLevel a b n) r f = f r (show a, n)

instance (IterLogFeature a, IterLogFeature b) => IterLogFeature (LFeaturePair a b) where
    iterLogFeature (LFeaturePair a b) r f = iterLogFeature b (iterLogFeature a r f) f

instance IterLogFeature () where
    iterLogFeature _ r _ = r

featureNameList = iterLogFeature logFeatureTree [] (\l f -> (f:l))

featureNameArray :: Array Int String
featureNameArray = array (1, logFeatureCount) $ map (\(name, LogFeature id) -> (id, name)) featureNameList

intToLevelName :: Int -> String
intToLevelName 0 = "Trace"
intToLevelName 1 = "Debug"
intToLevelName 2 = "Info"
intToLevelName 3 = "Warning"
intToLevelName 4 = "Error"
intToLevelName _ = "Unknown"

intToFeatureName :: Int -> String
intToFeatureName fId | fId > 0 && fId <= logFeatureCount = featureNameArray ! fId
intToFeatureName _ = "Unknown"

--------------------------------------------------------------------------------
