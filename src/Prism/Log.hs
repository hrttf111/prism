module Prism.Log (
    commonF
    , traceJmpIntra, traceJmpInter
    , traceCallNear, traceCallIntra, traceCallInter
    , traceRetIntra, traceRetInter
    , traceInterrupt, traceRetInterrupt
) where

import Data.Bits (shiftL)
import Numeric (showHex)

import Prism.Cpu

-------------------------------------------------------------------------------

commonF = LogFeature1 0

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
