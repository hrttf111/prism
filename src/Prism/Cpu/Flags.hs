{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prism.Cpu.Flags where

import Control.Monad.State.Strict (get, modify)
import Control.Monad.Trans (MonadIO, liftIO)

import Data.Bits (popCount, testBit, finiteBitSize, (.&.), (.|.))

import Prism.Cpu.Types
import Prism.Cpu.Monad
import Prism.Cpu.Registers
import Prism.Cpu.Val

-------------------------------------------------------------------------------

flagsToVal :: Flags -> Uint16 -> Uint16
flagsToVal (Flags cf pf af zf sf of_) = 
    (stf of_ 0x0800)
    . (stf sf 0x0080)
    . (stf zf 0x0040)
    . (stf af 0x0010)
    . (stf pf 0x0004)
    . (stf cf 0x0001)
    where
        stf flag bit val = if flag then val .|. bit else val

eflagsToVal :: EFlags -> Uint16 -> Uint16
eflagsToVal (EFlags tf if_ df) = 
    (stf tf 0x0100)
    . (stf if_ 0x0200)
    . (stf df 0x0400)
    where
        stf flag bit val = if flag then val .|. bit else val

valToFlags :: Uint16 -> Flags
valToFlags val = 
    Flags 
        (gtf 0x0001 val) 
        (gtf 0x0004 val)
        (gtf 0x0010 val)
        (gtf 0x0040 val)
        (gtf 0x0080 val)
        (gtf 0x0800 val)
    where
        gtf bit val = (val .&. bit) == bit

valToEFlags :: Uint16 -> EFlags
valToEFlags val =
    EFlags
        (gtf 0x0100 val)
        (gtf 0x0200 val)
        (gtf 0x0400 val) 
    where
        gtf bit val = (val .&. bit) == bit

regToFlags :: (MonadIO m) => MemReg -> Reg16 -> m (Flags, EFlags)
regToFlags memReg reg = do
    valReg <- readReg16 memReg reg
    return $ (valToFlags valReg, valToEFlags valReg)

readFlags :: (MonadIO m) => MemReg -> m (Flags, EFlags)
readFlags memReg = do
    val <- readRegFlags memReg
    return $ (valToFlags val, valToEFlags val)

-------------------------------------------------------------------------------

calcCFCarry :: (OperandVal b) => b -> b -> Bool
calcCFCarry before after = after < before

calcCFBorrow :: (OperandVal b) => b -> b -> Bool
calcCFBorrow before after = after > before

calcAFCarry :: (OperandVal b) => b -> b -> Bool
calcAFCarry before after = (after .&. 0x0F) < (before .&. 0x0F)

calcAFBorrow :: (OperandVal b) => b -> b -> Bool
calcAFBorrow before after = (after .&. 0x0F) > (before .&. 0x0F)

calcPF :: (OperandVal b) => b -> Bool
calcPF = even . popCount . (.&. 0xFF)

calcZF :: (OperandVal b) => b -> Bool
calcZF = (==0)

calcSF :: (OperandVal b) => b -> Bool
calcSF val = testBit val ((finiteBitSize val) - 1)

calcOFAdd :: (OperandVal b) => b -> b -> b -> Bool
calcOFAdd before val after | before < negV = 
    if val < negV then after >= negV else False
calcOFAdd before val after =
    if val >= negV then after < negV else False

calcOFSub :: (OperandVal b) => b -> b -> b -> Bool
calcOFSub before val after | before < negV =
    if val >= negV then after >= negV else False
calcOFSub before val after =
    if val < negV then after < negV else False

-------------------------------------------------------------------------------

getFlagVal :: Flag -> Flags -> Bool
getFlagVal CF = flagCF
getFlagVal PF = flagPF
getFlagVal AF = flagAF
getFlagVal ZF = flagZF
getFlagVal SF = flagSF
getFlagVal OF = flagOF

setFlagVal :: Flag -> Bool -> Flags -> Flags
setFlagVal CF v flags = flags { flagCF = v }
setFlagVal PF v flags = flags { flagPF = v }
setFlagVal AF v flags = flags { flagAF = v }
setFlagVal ZF v flags = flags { flagZF = v }
setFlagVal SF v flags = flags { flagSF = v }
setFlagVal OF v flags = flags { flagOF = v }

getEFlagVal :: EFlag -> EFlags -> Bool
getEFlagVal TF = eflagTF
getEFlagVal IF = eflagIF
getEFlagVal DF = eflagDF

setEFlagVal :: EFlag -> Bool -> EFlags -> EFlags
setEFlagVal TF v flags = flags { eflagTF = v }
setEFlagVal IF v flags = flags { eflagIF = v }
setEFlagVal DF v flags = flags { eflagDF = v }

-------------------------------------------------------------------------------

instance CpuFlag Flag CpuTrans where
    getFlag flag =
        ((getFlagVal flag) . ctxFlags) <$> get
    setFlag flag v =
        modify (\s -> s { ctxFlags = setFlagVal flag v (ctxFlags s) } )

instance CpuFlag EFlag CpuTrans where
    getFlag eflag =
        ((getEFlagVal eflag) . ctxEFlags) <$> get
    setFlag eflag v =
        modify (\s -> s { ctxEFlags = setEFlagVal eflag v (ctxEFlags s) } )

-------------------------------------------------------------------------------

instance CpuFlags Flags CpuTrans where
    getFlags = ctxFlags <$> get
    setFlags f = modify (\s -> s { ctxFlags = f } )

instance CpuFlags EFlags CpuTrans where
    getFlags = ctxEFlags <$> get
    setFlags f = modify (\s -> s { ctxEFlags = f } )

-------------------------------------------------------------------------------

type AllFlags = (Flags, EFlags)

instance MemOpManipulator RegSpec MemReg AllFlags where
    readOpRaw memReg _ = readFlags memReg

instance MemOpManipulator Flag MemReg Bool where
    readOpRaw memReg flag = do
        (getFlagVal flag) . fst <$> readFlags memReg

instance MemOpManipulator EFlag MemReg Bool where
    readOpRaw memReg flag = do
        (getEFlagVal flag) . snd <$> readFlags memReg

-------------------------------------------------------------------------------

showFlags :: Flags -> String
showFlags (Flags cf pf af zf sf of_) = "CF=" ++ (show cf) ++
                                       ", PF=" ++ (show pf) ++
                                       ", AF=" ++ (show af) ++
                                       ", ZF=" ++ (show zf) ++
                                       ", SF=" ++ (show sf) ++
                                       ", OF=" ++ (show of_)

showEFlags :: EFlags -> String
showEFlags (EFlags tf if_ df) = "TF=" ++ (show tf) ++
                                       ", IF=" ++ (show if_) ++
                                       ", DF=" ++ (show df)

printFlags :: MonadIO m => MemReg -> m [String]
printFlags memReg = do
    (flags, eflags) <- readFlags memReg
    return [
        showFlags flags,
        showEFlags eflags
        ]

-------------------------------------------------------------------------------
