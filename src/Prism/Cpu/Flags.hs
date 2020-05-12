{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Prism.Cpu.Flags where

import Control.Monad.State.Strict (get, modify)
import Control.Monad.Trans (MonadIO, liftIO)

import Data.Int (Int8)
import Data.Bits (popCount, testBit, finiteBitSize, (.&.), (.|.))

import Prism.Cpu.Types
import Prism.Cpu.Monad
import Prism.Cpu.Registers

-------------------------------------------------------------------------------

calcCFCarry8 :: Uint8 -> Uint8 -> Bool
calcCFCarry8 = calcCFCarry

calcCFBorrow8 :: Uint8 -> Uint8 -> Bool
calcCFBorrow8 = calcCFBorrow

calcCFCarry16 :: Uint16 -> Uint16 -> Bool
calcCFCarry16 = calcCFCarry

calcCFBorrow16 :: Uint16 -> Uint16 -> Bool
calcCFBorrow16 = calcCFBorrow

calcPF8 :: Uint8 -> Bool
calcPF8 = calcPF

calcPF16 :: Uint16 -> Bool
calcPF16 = calcPF

calcAFCarry8 :: Uint8 -> Uint8 -> Bool
calcAFCarry8 = calcAFCarry

calcAFBorrow8 :: Uint8 -> Uint8 -> Bool
calcAFBorrow8 = calcAFBorrow

calcAFCarry16 :: Uint16 -> Uint16 -> Bool
calcAFCarry16 = calcAFCarry

calcAFBorrow16 :: Uint16 -> Uint16 -> Bool
calcAFBorrow16 = calcAFBorrow

calcZF8 :: Uint8 -> Bool
calcZF8 = calcZF

calcZF16 :: Uint16 -> Bool
calcZF16 = calcZF

calcSF8 :: Uint8 -> Bool
calcSF8 = calcSF

calcSF16 :: Uint16 -> Bool
calcSF16 = calcSF

calcOFAdd8i :: Int8 -> Int8 -> Int8 -> Bool
calcOFAdd8i before val after | before >= 0 =
    if val >= 0 then before > after else False
calcOFAdd8i before val after =
    if val <= 0 then before < after else False

calcOFSub8i :: Int8 -> Int8 -> Int8 -> Bool
calcOFSub8i before val after | before >= 0 =
    if val <= 0 then before > after else False
calcOFSub8i before val after =
    if val >= 0 then before < after else False

calcOFAdd8 :: Uint8 -> Uint8 -> Uint8 -> Bool
calcOFAdd8 = calcOFAdd

calcOFSub8 :: Uint8 -> Uint8 -> Uint8 -> Bool
calcOFSub8 = calcOFSub

calcOFAdd16 :: Uint16 -> Uint16 -> Uint16 -> Bool
calcOFAdd16 = calcOFAdd

calcOFSub16 :: Uint16 -> Uint16 -> Uint16 -> Bool
calcOFSub16 = calcOFSub

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

regToFlags :: MonadIO m => MemReg -> Reg16 -> m (Flags, EFlags)
regToFlags memReg reg = do
    valReg <- readReg16 memReg reg
    return $ (valToFlags valReg, valToEFlags valReg)

readFlags :: MonadIO m => MemReg -> m (Flags, EFlags)
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

negV :: (OperandVal b) => b
negV = (div maxBound 2) + 1

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

instance CpuFlag Flag CpuTrans where
    getFlag CF =
        (flagCF . ctxFlags) <$> get
    getFlag PF =
        (flagPF . ctxFlags) <$> get
    getFlag AF =
        (flagAF . ctxFlags) <$> get
    getFlag ZF =
        (flagZF . ctxFlags) <$> get
    getFlag SF =
        (flagSF . ctxFlags) <$> get
    getFlag OF =
        (flagOF . ctxFlags) <$> get
    setFlag CF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagCF = b } } )
    setFlag PF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagPF = b } } )
    setFlag AF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagAF = b } } )
    setFlag ZF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagZF = b } } )
    setFlag SF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagSF = b } } )
    setFlag OF b =
        modify (\s -> s { ctxFlags = (ctxFlags s) { flagOF = b } } )

instance CpuFlag EFlag CpuTrans where
    getFlag TF =
        (eflagTF . ctxEFlags) <$> get
    getFlag IF =
        (eflagIF . ctxEFlags) <$> get
    getFlag DF =
        (eflagDF . ctxEFlags) <$> get
    setFlag TF b =
        modify (\s -> s { ctxEFlags = (ctxEFlags s) { eflagTF = b } } )
    setFlag IF b =
        modify (\s -> s { ctxEFlags = (ctxEFlags s) { eflagIF = b } } )
    setFlag DF b =
        modify (\s -> s { ctxEFlags = (ctxEFlags s) { eflagDF = b } } )

-------------------------------------------------------------------------------

instance CpuFlags Flags CpuTrans where
    getFlags = ctxFlags <$> get
    setFlags f = modify (\s -> s { ctxFlags = f } )

instance CpuFlags EFlags CpuTrans where
    getFlags = ctxEFlags <$> get
    setFlags f = modify (\s -> s { ctxEFlags = f } )

-------------------------------------------------------------------------------
