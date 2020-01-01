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

import Control.Exception (Exception, throwIO)
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Control.Concurrent.STM.TQueue
import Control.Monad.STM (atomically)
import Numeric (showHex)

import Foreign.Ptr
import Foreign.Storable (peekByteOff, pokeByteOff)
import Data.Array.Unboxed ((!))

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

getImm8 :: Uint8 -> Imm8
getImm8 = id

getImm16 :: Uint8 -> Uint8 -> Imm16
getImm16 lo hi = (+) (fromIntegral lo :: Imm16) $ shiftL (fromIntegral hi :: Imm16) 8

-------------------------------------------------------------------------------

instance Operand Reg8 Word8 where
    readOp ctx reg = readReg8 (ctxReg ctx) reg
    writeOp ctx reg val = writeReg8 (ctxReg ctx) reg val

instance RegDecoder Reg8 where
    decodeReg = Reg8
    decodeRegVal (Reg8 v) = v

instance Operand Reg16 Word16 where
    readOp ctx reg = readReg16 (ctxReg ctx) reg
    writeOp ctx reg val = writeReg16 (ctxReg ctx) reg val

instance RegDecoder Reg16 where
    decodeReg = Reg16
    decodeRegVal (Reg16 v) = v

instance Operand RegSeg Word16 where
    readOp ctx reg = readSeg (ctxReg ctx) reg
    writeOp ctx reg val = writeSeg (ctxReg ctx) reg val

instance RegDecoder RegSeg where
    decodeReg = RegSeg
    decodeRegVal (RegSeg v) = v

convertReg :: (OperandVal b1, OperandVal b2, OperandReg a1 b1, OperandReg a2 b2) =>
    a1 -> a2
convertReg = decodeReg . decodeRegVal

-------------------------------------------------------------------------------

data IOCtxException = IOCtxException deriving Show

instance Exception IOCtxException

instance IOVal Uint8 where
    ioValRead (IOQueue req rsp) cmdType offset = liftIO $ do
        val <- atomically $ do
            writeTQueue req $ IOCmdRead8 cmdType offset
            readTQueue rsp
        case val of
            IOCmdData8 d -> return d
            _ -> throwIO IOCtxException
    ioValWrite (IOQueue req _) cmdType offset val = liftIO $ do
        atomically $ writeTQueue req $ IOCmdWrite8 cmdType offset val
    ioValRespond (IOQueue _ rsp) val = liftIO $ do
        atomically $ writeTQueue rsp $ IOCmdData8 val

instance IOVal Uint16 where
    ioValRead (IOQueue req rsp) cmdType offset = liftIO $ do
        val <- atomically $ do
            writeTQueue req $ IOCmdRead16 cmdType offset
            readTQueue rsp
        case val of
            IOCmdData16 d -> return d
            _ -> throwIO IOCtxException
    ioValWrite (IOQueue req _) cmdType offset val = liftIO $ do
        atomically $ writeTQueue req $ IOCmdWrite16 cmdType offset val
    ioValRespond (IOQueue _ rsp) val = liftIO $ do
        atomically $ writeTQueue rsp $ IOCmdData16 val

-------------------------------------------------------------------------------

readMem :: (OperandVal b, IOVal b, MonadIO m) => Ctx -> MemOffset -> m b
readMem ctx offset =
    if isMemIOMapped (ctxIO ctx) offset then
            ioMemRead (ctxIO ctx) offset
        else 
            readMemMain (ctxMem ctx) offset
    where
        readMemMain (MemMain mm) offset = 
            liftIO $ peekByteOff mm offset

writeMem :: (OperandVal b, IOVal b, MonadIO m) => Ctx -> MemOffset -> b -> m ()
writeMem ctx offset val = 
    if isMemIOMapped (ctxIO ctx) offset then
            ioMemWrite (ctxIO ctx) offset val
        else 
            writeMemMain (ctxMem ctx) offset val
    where
        writeMemMain (MemMain mm) offset val =
            liftIO $ pokeByteOff mm offset val

instance Operand Mem8 Word8 where
    readOp ctx (Mem8 mem) = do
        offset <- getMemOffset1 ctx mem
        readMem ctx offset
    writeOp ctx (Mem8 mem) val = do
        offset <- getMemOffset1 ctx mem
        writeMem ctx offset val

instance MemDecoder Mem8 where
    decodeMem1 v off = Mem8 $ decodeMem v off
    decodeMemDirect = Mem8 . MemDirect
    unwrapMem (Mem8 m) = m
    wrapMem m = Mem8 m

instance OperandMem Mem8 Word8 where
    readMemOp ctx regSeg (Mem8 mem) = do
        offset <- getMemOffset2 ctx regSeg mem
        readMem ctx offset
    writeMemOp ctx regSeg (Mem8 mem) val = do
        offset <- getMemOffset2 ctx regSeg mem
        writeMem ctx offset val

instance Operand Mem16 Word16 where
    readOp ctx (Mem16 mem) = do
        offset <- getMemOffset1 ctx mem
        readMem ctx offset
    writeOp ctx (Mem16 mem) val = do
        offset <- getMemOffset1 ctx mem
        writeMem ctx offset val

instance MemDecoder Mem16 where
    decodeMem1 v off = Mem16 $ decodeMem v off
    decodeMemDirect = Mem16 . MemDirect
    unwrapMem (Mem16 m) = m
    wrapMem m = Mem16 m

instance OperandMem Mem16 Word16 where
    readMemOp ctx regSeg (Mem16 mem) = do
        offset <- getMemOffset2 ctx regSeg mem
        readMem ctx offset
    writeMemOp ctx regSeg (Mem16 mem) val = do
        offset <- getMemOffset2 ctx regSeg mem
        writeMem ctx offset val

convertMem :: (OperandVal b1, OperandVal b2, OperandMem a1 b1, OperandMem a2 b2) =>
    a1 -> a2
convertMem = wrapMem . unwrapMem

-------------------------------------------------------------------------------

isMemIOMapped :: IOCtx -> MemOffset -> Bool
isMemIOMapped ioCtx memOffset = inMemRegion (ioCtxMemRegion ioCtx)
    where
        pageNumber = div memOffset (ioCtxPageSize ioCtx)
        inMemRegion (MemIORegion arr) = arr ! pageNumber

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

getMemOffset1 :: MonadIO m => Ctx -> Mem -> m MemOffset
getMemOffset1 ctx mem = getMemOffset (ctxReg ctx) (ctxReplaceSeg ctx) mem

getMemOffset2 :: MonadIO m => Ctx -> RegSeg -> Mem -> m MemOffset
getMemOffset2 ctx regSeg mem = getMemOffset (ctxReg ctx) (Just regSeg) mem

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

writeMemSp :: (MonadIO m, OperandVal b) => MemReg -> MemMain -> b -> m () 
writeMemSp memReg (MemMain mm) val = do
    offset <- getMemReg2 memReg sp ss 0
    liftIO $ pokeByteOff mm offset val

readMemSp :: (MonadIO m, OperandVal b) => MemReg -> MemMain -> m b
readMemSp memReg (MemMain mm) = do
    offset <- getMemReg2 memReg sp ss 0
    liftIO $ peekByteOff mm offset

writeMemSp8 :: MonadIO m => MemReg -> MemMain -> Uint8 -> m () 
writeMemSp8 = writeMemSp

writeMemSp16 :: MonadIO m => MemReg -> MemMain -> Uint16 -> m () 
writeMemSp16 = writeMemSp

readMemSp8 :: MonadIO m => MemReg -> MemMain -> m Uint8
readMemSp8 = readMemSp

readMemSp16 :: MonadIO m => MemReg -> MemMain -> m Uint16
readMemSp16 = readMemSp

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

signExterndWordN :: (OperandVal b1, OperandVal b2) => b1 -> b2
signExterndWordN val | val > 0x80 = (+0xFF00) $ fromIntegral val
signExterndWordN val = fromIntegral val

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

type OperandFunc1 a b = (OperandVal b, Operand a b)
type OperandFunc2 a1 a2 b = (OperandVal b, Operand a1 b, Operand a2 b)

type FuncImplicit = Ctx -> PrismM

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

instrOp1ToOp2 :: OperandFunc2 a1 a2 b => FuncV2 b -> FuncO2M a1 a2
instrOp1ToOp2 = instrO2

instrOp2ToOp1 :: OperandFunc2 a1 a2 b => FuncV2 b -> FuncO2M a1 a2
instrOp2ToOp1 func ctx op1 op2 = instrO2 func ctx op2 op1

instrRmToReg :: OperandFunc2 a1 a2 b => FuncV2 b -> FuncO2M a1 a2
instrRmToReg = instrOp1ToOp2

instrRegToRm :: OperandFunc2 a1 a2 b => FuncV2 b -> FuncO2M a1 a2
instrRegToRm = instrOp2ToOp1

emptyRegReg :: Ctx -> a -> a -> PrismM
emptyRegReg ctx _ _ = return ctx

emptySingle :: Ctx -> a -> PrismM
emptySingle ctx _ = return ctx

-------------------------------------------------------------------------------
