{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module PrismCpu where

import Data.Word (Word8, Word16, Word32)
import Data.Maybe (fromMaybe)
import Data.Bits
import Data.Int

import Control.Monad.Trans (MonadIO, liftIO)
import Numeric (showHex)

import Foreign.Ptr
import Foreign.Storable (peekByteOff, pokeByteOff)

import Prism

-------------------------------------------------------------------------------

al = Reg8 0
cl = Reg8 1
dl = Reg8 2
bl = Reg8 3
ah = Reg8 4
ch = Reg8 5
dh = Reg8 6
bh = Reg8 7

instance Show Reg8 where
    show (Reg8 0) = "AL"
    show (Reg8 1) = "CL"
    show (Reg8 2) = "DL"
    show (Reg8 3) = "BL"
    show (Reg8 4) = "AH"
    show (Reg8 5) = "CH"
    show (Reg8 6) = "DH"
    show (Reg8 7) = "BH"

ax = Reg16 0
cx = Reg16 1
dx = Reg16 2
bx = Reg16 3
sp = Reg16 4
bp = Reg16 5
si = Reg16 6
di = Reg16 7

instance Show Reg16 where
    show (Reg16 0) = "AX"
    show (Reg16 1) = "CX"
    show (Reg16 2) = "DX"
    show (Reg16 3) = "BX"
    show (Reg16 4) = "SP"
    show (Reg16 5) = "BP"
    show (Reg16 6) = "SI"
    show (Reg16 7) = "DI"

es = RegSeg 0
cs = RegSeg 1
ss = RegSeg 2
ds = RegSeg 3

-------------------------------------------------------------------------------

instance ImmDecoder Imm8 where
    decodeImm b1 _ = getImm8 b1
    immLength _ = 1

instance ImmDecoder Imm16 where
    decodeImm = getImm16
    immLength _ = 2

-------------------------------------------------------------------------------

instance Operand Reg8 Word8 where
    readOp ctx reg = readReg8 (ctxReg ctx) reg
    writeOp ctx reg val = writeReg8 (ctxReg ctx) reg val

instance RegDecoder Reg8 where
    decodeReg = Reg8

instance Operand Reg16 Word16 where
    readOp ctx reg = readReg16 (ctxReg ctx) reg
    writeOp ctx reg val = writeReg16 (ctxReg ctx) reg val

instance RegDecoder Reg16 where
    decodeReg = Reg16

instance Operand RegSeg Word16 where
    readOp ctx reg = readSeg (ctxReg ctx) reg
    writeOp ctx reg val = writeSeg (ctxReg ctx) reg val

instance RegDecoder RegSeg where
    decodeReg = RegSeg

-------------------------------------------------------------------------------

instance Operand Mem8 Word8 where
    readOp ctx (Mem8 mem) = readMem8 (ctxReg ctx) (ctxMem ctx) (ctxReplaceSeg ctx) mem
    writeOp ctx (Mem8 mem) val = writeMem8 (ctxReg ctx) (ctxMem ctx) (ctxReplaceSeg ctx) mem val

instance MemDecoder Mem8 where
    decodeMem1 v off = Mem8 $ decodeMem v off
    decodeMemDirect = Mem8 . MemDirect

instance Operand Mem16 Word16 where
    readOp ctx (Mem16 mem) = readMem16 (ctxReg ctx) (ctxMem ctx) (ctxReplaceSeg ctx) mem
    writeOp ctx (Mem16 mem) val = writeMem16 (ctxReg ctx) (ctxMem ctx) (ctxReplaceSeg ctx) mem val

instance MemDecoder Mem16 where
    decodeMem1 v off = Mem16 $ decodeMem v off
    decodeMemDirect = Mem16 . MemDirect

-------------------------------------------------------------------------------

-- R/M -> Disp -> Mem
decodeMem :: Uint8 -> Disp -> Mem
decodeMem 0 = MemBxSi 
decodeMem 1 = MemBxDi
decodeMem 2 = MemBpSi
decodeMem 3 = MemBpDi
decodeMem 4 = MemSi
decodeMem 5 = MemDi
decodeMem 6 = MemBp
decodeMem 7 = MemBx
decodeMem _ = MemDirect

getDisp8 :: Uint8 -> Disp
getDisp8 lo = fromIntegral lo :: Disp

getDisp16 :: Uint8 -> Uint8 -> Disp
getDisp16 lo hi = (+) (fromIntegral lo :: Disp) $ shiftL (fromIntegral hi :: Disp) 8

getImm8 :: Uint8 -> Imm8
getImm8 = id

getImm16 :: Uint8 -> Uint8 -> Imm16
getImm16 lo hi = (+) (fromIntegral lo :: Imm16) $ shiftL (fromIntegral hi :: Imm16) 8

-------------------------------------------------------------------------------

readReg8 :: MonadIO m => MemReg -> Reg8 -> m Uint8
readReg8 (MemReg mr) (Reg8 ii) | ii < 4 =
    let i = fromIntegral ii in
    liftIO $ peekByteOff mr (i * 2)
readReg8 (MemReg mr) (Reg8 ii) = 
    let i = fromIntegral ii in
    liftIO $ peekByteOff mr ((i - 4) * 2 + 1)

writeReg8 :: MonadIO m => MemReg -> Reg8 -> Uint8 -> m ()
writeReg8 (MemReg mr) (Reg8 ii) val | ii < 4 =
    let i = fromIntegral ii in
    liftIO $ pokeByteOff mr (i * 2) val
writeReg8 (MemReg mr) (Reg8 ii) val =
    let i = fromIntegral ii in
    liftIO $ pokeByteOff mr ((i - 4) * 2 + 1) val

readReg16 :: MonadIO m => MemReg -> Reg16 -> m Uint16
readReg16 (MemReg mr) (Reg16 ii) =
    let i = fromIntegral ii in
    liftIO $ peekByteOff mr (i * 2)

writeReg16 :: MonadIO m => MemReg -> Reg16 -> Uint16 -> m ()
writeReg16 (MemReg mr) (Reg16 ii) val =
    let i = fromIntegral ii in
    liftIO $ pokeByteOff mr (i * 2) val

readSeg :: MonadIO m => MemReg -> RegSeg -> m Uint16
readSeg (MemReg mr) (RegSeg ii) =
    let i = fromIntegral ii in
    liftIO $ peekByteOff mr (32 + i * 2)

writeSeg :: MonadIO m => MemReg -> RegSeg -> Uint16 -> m ()
writeSeg (MemReg mr) (RegSeg ii) val =
    let i = fromIntegral ii in
    liftIO $ pokeByteOff mr (32 + i * 2) val

readRegIP :: MonadIO m => MemReg -> m Uint16
readRegIP (MemReg mr) = liftIO $ peekByteOff mr 40

writeRegIP :: MonadIO m => MemReg -> Uint16 -> m ()
writeRegIP (MemReg mr) val = liftIO $ pokeByteOff mr 40 val

-------------------------------------------------------------------------------

showReg8 :: MonadIO m => MemReg -> Reg8 -> m String
showReg8 memReg reg = do
    val <- readReg8 memReg reg
    return $ show reg ++ " = 0x" ++ showHex val ""

showReg16 :: MonadIO m => MemReg -> Reg16 -> m String
showReg16 memReg reg = do
    val <- readReg16 memReg reg
    return $ show reg ++ " = 0x" ++ showHex val ""

showRegSeg :: MonadIO m => MemReg -> RegSeg -> m String
showRegSeg memReg reg = do
    val <- readSeg memReg reg
    return $ show reg ++ " = 0x" ++ showHex val ""

showRegs3 :: MonadIO m => MemReg -> Reg8 -> Reg8 -> Reg16 -> m String
showRegs3 memReg reg1 reg2 reg3 = do
    s1 <- showReg8 memReg reg1
    s2 <- showReg8 memReg reg2
    s3 <- showReg16 memReg reg3
    return $ s1 ++ " " ++ s2 ++ " " ++ s3

showRegIP :: MonadIO m => MemReg -> m String
showRegIP memReg = do
    val <- readRegIP memReg
    return $ "IP = 0x" ++ showHex val ""

printRegs :: MonadIO m => MemReg -> m ()
printRegs memReg = do
    (liftIO . putStrLn) =<< showRegs3 memReg al ah ax
    (liftIO . putStrLn) =<< showRegs3 memReg bl bh bx
    (liftIO . putStrLn) =<< showRegs3 memReg cl ch cx
    (liftIO . putStrLn) =<< showRegs3 memReg dl dh dx
    (liftIO . putStrLn) =<< showReg16 memReg sp
    (liftIO . putStrLn) =<< showReg16 memReg bp
    (liftIO . putStrLn) =<< showReg16 memReg si
    (liftIO . putStrLn) =<< showReg16 memReg di
    (liftIO . putStrLn) =<< showRegSeg memReg es
    (liftIO . putStrLn) =<< showRegSeg memReg cs
    (liftIO . putStrLn) =<< showRegSeg memReg ss
    (liftIO . putStrLn) =<< showRegSeg memReg ds
    (liftIO . putStrLn) =<< showRegIP memReg

-------------------------------------------------------------------------------

getEA3 :: MonadIO m => MemReg -> Reg16 -> Reg16 -> Disp -> m EA
getEA3 memReg reg1 reg2 disp = do
    valR1 <- fromIntegral <$> readReg16 memReg reg1
    valR2 <- fromIntegral <$> readReg16 memReg reg2
    return $ valR1 + valR2 + disp

getEA2 :: MonadIO m => MemReg -> Reg16 -> Disp -> m EA
getEA2 memReg reg1 disp = do
    valR1 <- readReg16 memReg reg1
    return $ valR1 + disp

getEA :: MonadIO m => MemReg -> Mem -> m EA
getEA memReg (MemBxSi disp) = getEA3 memReg bx si disp
getEA memReg (MemBxDi disp) = getEA3 memReg bx di disp
getEA memReg (MemBpSi disp) = getEA3 memReg bp si disp
getEA memReg (MemBpDi disp) = getEA3 memReg bp di disp
getEA memReg (MemSi disp) = getEA2 memReg si disp
getEA memReg (MemDi disp) = getEA2 memReg di disp
getEA memReg (MemBp disp) = getEA2 memReg bp disp
getEA memReg (MemBx disp) = getEA2 memReg bx disp
getEA memReg (MemDirect disp) = return disp

getMemReg3 :: MonadIO m => MemReg -> Reg16 -> Reg16 -> RegSeg -> Disp -> m MemOffset
getMemReg3 memReg reg1 reg2 regSeg disp = do
    valR1 <- fromIntegral <$> readReg16 memReg reg1
    valR2 <- fromIntegral <$> readReg16 memReg reg2
    valSeg <- fromIntegral <$> readSeg memReg regSeg
    let disp32 = fromIntegral disp :: MemOffset
    return $ (shiftL valSeg 4) + valR1 + valR2 + disp32

getMemReg2 :: MonadIO m => MemReg -> Reg16 -> RegSeg -> Disp -> m MemOffset
getMemReg2 memReg reg1 regSeg disp = do
    valR1 <- fromIntegral <$> readReg16 memReg reg1
    valSeg <- fromIntegral <$> readSeg memReg regSeg
    let disp32 = fromIntegral disp :: MemOffset
    return $ (shiftL valSeg 4) + valR1 + disp32

getMemReg1 :: MonadIO m => MemReg -> RegSeg -> Disp -> m MemOffset
getMemReg1 memReg regSeg disp = do
    valSeg <- fromIntegral <$> readSeg memReg regSeg
    let disp32 = fromIntegral disp :: MemOffset
    return $ (shiftL valSeg 4) + disp32

findRegSeg :: RegSeg -> Ctx -> RegSeg
findRegSeg defaultRegSeg ctx = getRegSeg (ctxReplaceSeg ctx) 
    where
        getRegSeg Nothing = defaultRegSeg
        getRegSeg (Just s) = s

findRegSegData :: Ctx -> Maybe RegSeg
findRegSegData = ctxReplaceSeg

findRegSeg1 :: RegSeg -> Maybe RegSeg -> RegSeg
findRegSeg1 = fromMaybe

getMemOffset :: MonadIO m => MemReg -> Maybe RegSeg -> Mem -> m MemOffset
getMemOffset memReg regSeg (MemBxSi disp) = getMemReg3 memReg bx si (findRegSeg1 ds regSeg) disp
getMemOffset memReg regSeg (MemBxDi disp) = getMemReg3 memReg bx di (findRegSeg1 ds regSeg) disp
getMemOffset memReg regSeg (MemBpSi disp) = getMemReg3 memReg bp si (findRegSeg1 ss regSeg) disp
getMemOffset memReg regSeg (MemBpDi disp) = getMemReg3 memReg bp di (findRegSeg1 ss regSeg) disp
getMemOffset memReg regSeg (MemSi disp) = getMemReg2 memReg si (findRegSeg1 ds regSeg) disp
getMemOffset memReg regSeg (MemDi disp) = getMemReg2 memReg di (findRegSeg1 ds regSeg) disp
getMemOffset memReg regSeg (MemBp disp) = getMemReg2 memReg bp (findRegSeg1 ss regSeg) disp
getMemOffset memReg regSeg (MemBx disp) = getMemReg2 memReg bx (findRegSeg1 ds regSeg) disp
getMemOffset memReg regSeg (MemDirect disp) = getMemReg1 memReg (findRegSeg1 ds regSeg) disp

readMem8 :: MonadIO m => MemReg -> MemMain -> Maybe RegSeg -> Mem -> m Uint8
readMem8 memReg (MemMain mm) regSeg mem = do
    offset <- getMemOffset memReg regSeg mem
    liftIO $ peekByteOff mm offset

writeMem8 :: MonadIO m => MemReg -> MemMain -> Maybe RegSeg -> Mem -> Uint8 -> m ()
writeMem8 memReg (MemMain mm) regSeg mem val = do
    offset <- getMemOffset memReg regSeg mem
    liftIO $ pokeByteOff mm offset val

readMem16 :: MonadIO m => MemReg -> MemMain -> Maybe RegSeg -> Mem -> m Uint16
readMem16 memReg (MemMain mm) regSeg mem = do
    offset <- getMemOffset memReg regSeg mem
    liftIO $ peekByteOff mm offset

writeMem16 :: MonadIO m => MemReg -> MemMain -> Maybe RegSeg -> Mem -> Uint16 -> m ()
writeMem16 memReg (MemMain mm) regSeg mem val = do
    offset <- getMemOffset memReg regSeg mem
    liftIO $ pokeByteOff mm offset val

readMem32 :: MonadIO m => MemReg -> MemMain -> Maybe RegSeg -> Mem -> m (Uint16, Uint16)
readMem32 memReg (MemMain mm) regSeg mem = do
    offset <- getMemOffset memReg regSeg mem
    val1 <- liftIO $ peekByteOff mm offset
    val2 <- liftIO $ peekByteOff mm (offset + 2)
    return (val1, val2)

readMemDirect32 :: MonadIO m => MemMain -> MemOffset -> m (Uint16, Uint16)
readMemDirect32 (MemMain mm) offset = do
    val1 <- liftIO $ peekByteOff mm offset
    val2 <- liftIO $ peekByteOff mm (offset + 2)
    return (val1, val2)

writeMemSp8 :: MonadIO m => MemReg -> MemMain -> Uint8 -> m () 
writeMemSp8 memReg (MemMain mm) val = do
    offset <- getMemReg2 memReg sp ss 0
    liftIO $ pokeByteOff mm offset val

writeMemSp16 :: MonadIO m => MemReg -> MemMain -> Uint16 -> m () 
writeMemSp16 memReg (MemMain mm) val = do
    offset <- getMemReg2 memReg sp ss 0
    liftIO $ pokeByteOff mm offset val

readMemSp8 :: MonadIO m => MemReg -> MemMain -> m Uint8
readMemSp8 memReg (MemMain mm) = do
    offset <- getMemReg2 memReg sp ss 0
    liftIO $ peekByteOff mm offset

readMemSp16 :: MonadIO m => MemReg -> MemMain -> m Uint16
readMemSp16 memReg (MemMain mm) = do
    offset <- getMemReg2 memReg sp ss 0
    liftIO $ peekByteOff mm offset

showMem3 :: Reg16 -> Reg16 -> RegSeg -> Disp -> String
showMem3 reg1 reg2 regSeg@(RegSeg 3) 0 =
    "[" ++ (show reg1) ++ " + " ++ (show reg2) ++ "]"
showMem3 reg1 reg2 regSeg@(RegSeg 3) disp =
    "[" ++ (show reg1) ++ " + " ++ (show reg2) ++ " + 0x" ++ (showHex disp "]")
showMem3 reg1 reg2 regSeg 0 =
    "[" ++ (show regSeg) ++ ":" ++ (show reg1) ++ " + " ++ (show reg2) ++ "]"
showMem3 reg1 reg2 regSeg disp =
    "[" ++ (show regSeg) ++ ":" ++ (show reg1) ++ " + " ++ (show reg2) ++ " + 0x" ++ (showHex disp "]")

showMem2 :: Reg16 -> RegSeg -> Disp -> String
showMem2 reg1 regSeg@(RegSeg 3) 0 =
    "[" ++ (show reg1) ++  "]"
showMem2 reg1 regSeg@(RegSeg 3) disp =
    "[" ++ (show reg1) ++ " + 0x" ++ (showHex disp "]")
showMem2 reg1 regSeg 0 =
    "[" ++ (show regSeg) ++ ":" ++ (show reg1) ++ "]"
showMem2 reg1 regSeg disp =
    "[" ++ (show regSeg) ++ ":" ++ (show reg1) ++ " + 0x" ++ (showHex disp "]")

showMem1 :: RegSeg -> Disp -> String
showMem1 regSeg@(RegSeg 3) disp =
    "[0x" ++ (showHex disp "]")
showMem1 regSeg disp =
    "[" ++ (show regSeg) ++ ":0x" ++ (showHex disp "]")

instance Show Mem where
    show (MemBxSi disp) = showMem3 bx si ds disp
    show (MemBxDi disp) = showMem3 bx di ds disp
    show (MemBpSi disp) = showMem3 bp si ss disp
    show (MemBpDi disp) = showMem3 bp di ss disp
    show (MemSi disp) = showMem2 si ds disp
    show (MemDi disp) = showMem2 di ds disp
    show (MemBp disp) = showMem2 bp ss disp
    show (MemBx disp) = showMem2 bx ds disp
    show (MemDirect disp) = showMem1 ds disp

-------------------------------------------------------------------------------

calcCFCarry8 :: Uint8 -> Uint8 -> Bool
calcCFCarry8 before after = after < before

calcCFBorrow8 :: Uint8 -> Uint8 -> Bool
calcCFBorrow8 before after = after > before

calcCFCarry16 :: Uint16 -> Uint16 -> Bool
calcCFCarry16 before after = after < before

calcCFBorrow16 :: Uint16 -> Uint16 -> Bool
calcCFBorrow16 before after = after > before

calcPF8 :: Uint8 -> Bool
calcPF8 = even . popCount

calcPF16 :: Uint16 -> Bool
calcPF16 = calcPF8 . fromIntegral

calcAFCarry8 :: Uint8 -> Uint8 -> Bool
calcAFCarry8 before after = (after .&. 0x0F) < (before .&. 0x0F)

calcAFBorrow8 :: Uint8 -> Uint8 -> Bool
calcAFBorrow8 before after = (after .&. 0x0F) > (before .&. 0x0F)

calcAFCarry16 :: Uint16 -> Uint16 -> Bool
calcAFCarry16 before after = (after .&. 0x0F) < (before .&. 0x0F)

calcAFBorrow16 :: Uint16 -> Uint16 -> Bool
calcAFBorrow16 before after = (after .&. 0x0F) > (before .&. 0x0F)

calcZF8 :: Uint8 -> Bool
calcZF8 = (==0)

calcZF16 :: Uint16 -> Bool
calcZF16 = (==0)

calcSF8 :: Uint8 -> Bool
calcSF8 = (==0x80) . (.&. 0x80) 

calcSF16 :: Uint16 -> Bool
calcSF16 = (==0x8000) . (.&. 0x8000) 

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
calcOFAdd8 before val after | before < 0x80 =
    if val < 0x80 then after >= 0x80 else False
calcOFAdd8 before val after =
    if val >= 0x80 then after < 0x80 else False

calcOFSub8 :: Uint8 -> Uint8 -> Uint8 -> Bool
calcOFSub8 before val after | before < 0x80 =
    if val >= 0x80 then after >= 0x80 else False
calcOFSub8 before val after =
    if val < 0x80 then after < 0x80 else False

calcOFAdd16 :: Uint16 -> Uint16 -> Uint16 -> Bool
calcOFAdd16 before val after | before < 0x8000 =
    if val < 0x8000 then after >= 0x8000 else False
calcOFAdd16 before val after =
    if val >= 0x8000 then after < 0x8000 else False

calcOFSub16 :: Uint16 -> Uint16 -> Uint16 -> Bool
calcOFSub16 before val after | before < 0x8000 =
    if val >= 0x8000 then after >= 0x8000 else False
calcOFSub16 before val after =
    if val < 0x8000 then after < 0x8000 else False

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
readFlags (MemReg mr) = do
    val <- liftIO $ peekByteOff mr 42
    return $ (valToFlags val, valToEFlags val)

-------------------------------------------------------------------------------

getInstrAddress :: MonadIO m => MemReg -> RegSeg -> Uint16 -> m MemOffset
getInstrAddress memReg regSeg ip = do
    valSeg <- fromIntegral <$> readSeg memReg regSeg
    let ip32 = fromIntegral ip :: MemOffset
    return $ (shiftL valSeg 4) + ip32

moveRegIP :: MonadIO m => MemReg -> Uint16 -> m ()
moveRegIP memReg val = do
    currentVal <- readRegIP memReg
    writeRegIP memReg (currentVal + val)

updateIP :: MonadIO m => Uint16 -> Ctx -> m Ctx
updateIP val ctx = moveRegIP (ctxReg ctx) val >> return ctx

-------------------------------------------------------------------------------

signExterndWord :: Uint8 -> Uint16
signExterndWord val | val > 0x80 = (+0xFF00) $ fromIntegral val
signExterndWord val = fromIntegral val

signExterndDoubleword :: Uint16 -> (Uint16, Uint16)
signExterndDoubleword val | val > 0x8000 = (0xFFFF, val)
signExterndDoubleword val = (0x0000, val)

signExterndDoubleword32 :: Uint16 -> Uint32
signExterndDoubleword32 val | val > 0x8000 = (+0xFFFF0000) $ fromIntegral val
signExterndDoubleword32 val = fromIntegral val

toSignedCompl2 :: (OperandVal a, OperandVal b) => a -> b
toSignedCompl2 val = valUnsigned - valSigned
    where
        upperBit = (finiteBitSize val) - 1
        valUnsigned = fromIntegral $ clearBit val upperBit
        valSigned = if testBit val upperBit then bit upperBit else 0

toUnsignedComp2 :: (OperandVal a, OperandVal b) => a -> b
toUnsignedComp2 = toSignedCompl2

signedOp :: (OperandVal a, OperandVal b) => (b -> b -> b) -> a -> a -> a
signedOp func val1 val2 = toUnsignedComp2 $ func sVal1 sVal2
    where
        sVal1 = toSignedCompl2 val1
        sVal2 = toSignedCompl2 val2

signedOp1 :: (OperandVal a, OperandVal b) => (b -> b -> b) -> a -> a -> b
signedOp1 func val1 val2 = func sVal1 sVal2
    where
        sVal1 = toSignedCompl2 val1
        sVal2 = toSignedCompl2 val2

signedOpS :: (OperandVal a, OperandVal b) => (b -> b) -> a -> a
signedOpS func val1 = toUnsignedComp2 $ func $ toSignedCompl2 val1

-------------------------------------------------------------------------------

type FuncImplicit = Ctx -> PrismM
type FuncImm8 = Ctx -> Imm8 -> PrismM
type FuncImm16 = Ctx -> Imm16 -> PrismM
type FuncImm32 = Ctx -> Imm16 -> Imm16 -> PrismM

type FuncReg8 = Ctx -> Reg8 -> PrismM
type FuncReg16 = Ctx -> Reg16 -> PrismM
type FuncSeg = Ctx -> RegSeg -> PrismM

type FuncMem = Ctx -> Mem -> PrismM

type FuncRegImm8 = Ctx -> Reg8 -> Imm8 -> PrismM
type FuncMemImm8 = Ctx -> Mem -> Imm8 -> PrismCtx IO Ctx

type FuncRegImm16 = Ctx -> Reg16 -> Imm16 -> PrismCtx IO Ctx
type FuncMemImm16 = Ctx -> Mem -> Imm16 -> PrismCtx IO Ctx

type FuncRegReg8 = Ctx -> Reg8 -> Reg8 -> PrismCtx IO Ctx
type FuncRegReg16 = Ctx -> Reg16 -> Reg16 -> PrismCtx IO Ctx

type FuncMemReg8 = Ctx -> Mem -> Reg8 -> PrismCtx IO Ctx
type FuncMemReg16 = Ctx -> Mem -> Reg16 -> PrismCtx IO Ctx

type FuncSegImm16 = Ctx -> RegSeg -> Imm16 -> PrismM
type FuncSegReg16 = Ctx -> Reg16 -> RegSeg -> PrismM
type FuncMemSeg16 = Ctx -> Mem -> RegSeg -> PrismM

emptyRegReg :: Ctx -> a -> a -> PrismM
emptyRegReg ctx _ _ = return ctx

emptySingle :: Ctx -> a -> PrismM
emptySingle ctx _ = return ctx

-------------------------------------------------------------------------------

type OperandFunc1 a b = (OperandVal b, Operand a b)
type OperandFunc2 a1 a2 b = (OperandVal b, Operand a1 b, Operand a2 b)

type FuncImm1 i = Ctx -> i -> PrismM
type FuncImm2 i = Ctx -> i -> i -> PrismM

type FuncO1M a = Ctx -> a -> PrismM
type FuncO2M a1 a2 = Ctx -> a1 -> a2 -> PrismM

type FuncOI1M a b = Ctx -> a -> b -> PrismM

type FuncV1M b = Ctx -> b -> PrismCtx IO (Ctx, b)
type FuncNV1M b = Ctx -> b -> PrismM

type FuncV1 b = Ctx -> b -> (Ctx, b)
type FuncV2 b = Ctx -> b -> b -> (Ctx, b)

instrOV1 :: OperandFunc1 a b => FuncV1M b -> FuncO1M a
instrOV1 func ctx op = do
    val <- readOp ctx op
    (ctxNew, valNew) <- func ctx val
    writeOp ctxNew op valNew
    return ctxNew

{-# SPECIALISE instrOV1 :: FuncV1M Word8 -> FuncO1M Reg8 #-}
{-# SPECIALISE instrOV1 :: FuncV1M Word16 -> FuncO1M Reg16 #-}
{-# SPECIALISE instrOV1 :: FuncV1M Word16 -> FuncO1M RegSeg #-}
{-# SPECIALISE instrOV1 :: FuncV1M Word8 -> FuncO1M Mem8 #-}
{-# SPECIALISE instrOV1 :: FuncV1M Word16 -> FuncO1M Mem16 #-}

instrON1 :: OperandFunc1 a b => FuncNV1M b -> FuncO1M a
instrON1 func ctx op =
    readOp ctx op >>= func ctx

{-# SPECIALISE instrON1 :: FuncNV1M Word8 -> FuncO1M Reg8 #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 -> FuncO1M Reg16 #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 -> FuncO1M RegSeg #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word8 -> FuncO1M Mem8 #-}
{-# SPECIALISE instrON1 :: FuncNV1M Word16 -> FuncO1M Mem16 #-}

instrO1 :: OperandFunc1 a b => FuncV1 b -> FuncO1M a
instrO1 func ctx op = do
    val <- readOp ctx op
    let (ctxNew, valNew) = func ctx val
    writeOp ctxNew op valNew
    return ctxNew

{-# SPECIALISE instrO1 :: FuncV1 Word8 -> FuncO1M Reg8 #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M Reg16 #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M RegSeg #-}
{-# SPECIALISE instrO1 :: FuncV1 Word8 -> FuncO1M Mem8 #-}
{-# SPECIALISE instrO1 :: FuncV1 Word16 -> FuncO1M Mem16 #-}

instrOI1 :: OperandFunc1 a b => FuncV2 b -> FuncOI1M a b
instrOI1 func ctx op imm = do
    val <- readOp ctx op
    let (ctxNew, valNew) = func ctx imm val
    writeOp ctxNew op valNew
    return ctxNew

{-# SPECIALISE instrOI1 :: FuncV2 Word8 -> FuncOI1M Reg8 Word8 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M Reg16 Word16 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M RegSeg Word16 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word8 -> FuncOI1M Mem8 Word8 #-}
{-# SPECIALISE instrOI1 :: FuncV2 Word16 -> FuncOI1M Mem16 Word16 #-}

instrO2 :: OperandFunc2 a1 a2 b => FuncV2 b -> FuncO2M a1 a2
instrO2 func ctx op1 op2 = do
    val1 <- readOp ctx op1
    val2 <- readOp ctx op2
    let (ctxNew, valNew) = func ctx val1 val2
    writeOp ctx op2 valNew
    return ctxNew

{-# SPECIALISE instrO2 :: FuncV2 Word8 -> FuncO2M Reg8 Reg8 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M Reg16 Reg16 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word8 -> FuncO2M Mem8 Reg8 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word8 -> FuncO2M Reg8 Mem8#-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M Reg16 Mem16 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M Mem16 Reg16 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M RegSeg Reg16 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M Reg16 RegSeg #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M RegSeg Mem16 #-}
{-# SPECIALISE instrO2 :: FuncV2 Word16 -> FuncO2M Mem16 RegSeg #-}

-------------------------------------------------------------------------------

type FuncVal8 = Ctx -> Uint8 -> PrismCtx IO (Ctx, Uint8)
type FuncVal16 = Ctx -> Uint16 -> PrismCtx IO (Ctx, Uint16)

instrRegVal8 :: FuncVal8 -> FuncReg8
instrRegVal8 = instrOV1

instrRegVal16 :: FuncVal16 -> FuncReg16
instrRegVal16 = instrOV1

instrMemVal8 :: FuncVal8 -> FuncMem
instrMemVal8 func ctx mem = instrOV1 func ctx $ Mem8 mem

instrMemVal16 :: FuncVal16 -> FuncMem
instrMemVal16 func ctx mem = instrOV1 func ctx $ Mem16 mem

type FuncNoVal8 = Ctx -> Uint8 -> PrismM
type FuncNoVal16 = Ctx -> Uint16 -> PrismM

instrRegNoVal8 :: FuncNoVal8 -> FuncReg8
instrRegNoVal8 = instrON1

instrRegNoVal16 :: FuncNoVal16 -> FuncReg16
instrRegNoVal16 = instrON1

instrMemNoVal8 :: FuncNoVal8 -> FuncMem
instrMemNoVal8 func ctx mem = instrON1 func ctx $ Mem8 mem

instrMemNoVal16 :: FuncNoVal16 -> FuncMem
instrMemNoVal16 func ctx mem = instrON1 func ctx $ Mem16 mem

-------------------------------------------------------------------------------

-- ctx -> source -> dest -> (ctx, result)
type Func8To8 = Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
type Func16To16 = Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)

type Func8 = Ctx -> Uint8 -> (Ctx, Uint8)
type Func16 = Ctx -> Uint16 -> (Ctx, Uint16)

instrReg8 :: Func8 -> FuncReg8
instrReg8 func ctx reg = instrO1 func ctx reg

instrReg16 :: Func16 -> FuncReg16
instrReg16 func ctx reg = instrO1 func ctx reg

instrMem8 :: Func8 -> FuncMem
instrMem8 func ctx mem = instrO1 func ctx $ Mem8 mem

instrMem16 :: Func16 -> FuncMem
instrMem16 func ctx mem = instrO1 func ctx $ Mem16 mem

instrRegImm8 :: Func8To8 -> FuncRegImm8
instrRegImm8 = instrOI1

instrRegImm16 :: Func16To16 -> FuncRegImm16
instrRegImm16 = instrOI1

instrRegToReg8 :: Func8To8 -> FuncRegReg8
instrRegToReg8 = instrO2

instrRegToReg8RmToReg :: Func8To8 -> FuncRegReg8
instrRegToReg8RmToReg func ctx reg1 reg2 = instrRegToReg8 func ctx reg1 reg2

instrRegToReg8RegToRm :: Func8To8 -> FuncRegReg8
instrRegToReg8RegToRm func ctx reg1 reg2 = instrRegToReg8 func ctx reg2 reg1

instrRegToReg16 :: Func16To16 -> FuncRegReg16
instrRegToReg16 = instrO2

instrRegToReg16RmToReg :: Func16To16 -> FuncRegReg16
instrRegToReg16RmToReg func ctx reg1 reg2 = instrRegToReg16 func ctx reg1 reg2

instrRegToReg16RegToRm :: Func16To16 -> FuncRegReg16
instrRegToReg16RegToRm func ctx reg1 reg2 = instrRegToReg16 func ctx reg2 reg1

instrMemImm8 :: Func8To8 -> FuncMemImm8
instrMemImm8 func ctx mem imm = instrOI1 func ctx (Mem8 mem) imm

instrMemImm16 :: Func16To16 -> FuncMemImm16
instrMemImm16 func ctx mem imm = instrOI1 func ctx (Mem16 mem) imm

instrRegToMem8 :: Func8To8 -> FuncMemReg8
instrRegToMem8 func ctx mem reg = instrO2 func ctx reg (Mem8 mem)

instrRegToMem16 :: Func16To16 -> FuncMemReg16
instrRegToMem16 func ctx mem reg = instrO2 func ctx reg (Mem16 mem)

instrMemToReg8 :: Func8To8 -> FuncMemReg8
instrMemToReg8 func ctx mem reg = instrO2 func ctx (Mem8 mem) reg

instrMemToReg16 :: Func16To16 -> FuncMemReg16
instrMemToReg16 func ctx mem reg = instrO2 func ctx (Mem16 mem) reg

instrSegImm16 :: Func16To16 -> FuncSegImm16
instrSegImm16 = instrOI1

instrRegToSeg16 :: Func16To16 -> FuncSegReg16
instrRegToSeg16 func ctx reg regSeg = instrO2 func ctx reg regSeg

instrSegToReg16 :: Func16To16 -> FuncSegReg16
instrSegToReg16 func ctx reg regSeg = instrO2 func ctx regSeg reg

instrMemToSeg16 :: Func16To16 -> FuncMemSeg16
instrMemToSeg16 func ctx mem regSeg = instrO2 func ctx (Mem16 mem) regSeg

instrSegToMem16 :: Func16To16 -> FuncMemSeg16
instrSegToMem16 func ctx mem regSeg = instrO2 func ctx regSeg (Mem16 mem)
