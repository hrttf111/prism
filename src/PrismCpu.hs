{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module PrismCpu where

import Data.Bits ((.&.), (.|.), shiftR, shiftL, popCount, testBit)

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

--todo: EA for BP-based addresses is SS, not DS
getMemOffset :: MonadIO m => MemReg -> RegSeg -> Mem -> m MemOffset
getMemOffset memReg regSeg (MemBxSi disp) = getMemReg3 memReg bx si regSeg disp
getMemOffset memReg regSeg (MemBxDi disp) = getMemReg3 memReg bx di regSeg disp
getMemOffset memReg regSeg (MemBpSi disp) = getMemReg3 memReg bp si regSeg disp
getMemOffset memReg regSeg (MemBpDi disp) = getMemReg3 memReg bp di regSeg disp
getMemOffset memReg regSeg (MemSi disp) = getMemReg2 memReg si regSeg disp
getMemOffset memReg regSeg (MemDi disp) = getMemReg2 memReg di regSeg disp
getMemOffset memReg regSeg (MemBp disp) = getMemReg2 memReg bp regSeg disp
getMemOffset memReg regSeg (MemBx disp) = getMemReg2 memReg bx regSeg disp
getMemOffset memReg regSeg (MemDirect disp) = getMemReg1 memReg regSeg disp

readMem8 :: MonadIO m => MemReg -> MemMain -> RegSeg -> Mem -> m Uint8
readMem8 memReg (MemMain mm) regSeg mem = do
    offset <- getMemOffset memReg regSeg mem
    liftIO $ peekByteOff mm offset

writeMem8 :: MonadIO m => MemReg -> MemMain -> RegSeg -> Mem -> Uint8 -> m ()
writeMem8 memReg (MemMain mm) regSeg mem val = do
    offset <- getMemOffset memReg regSeg mem
    liftIO $ pokeByteOff mm offset val

readMem16 :: MonadIO m => MemReg -> MemMain -> RegSeg -> Mem -> m Uint16
readMem16 memReg (MemMain mm) regSeg mem = do
    offset <- getMemOffset memReg regSeg mem
    liftIO $ peekByteOff mm offset

writeMem16 :: MonadIO m => MemReg -> MemMain -> RegSeg -> Mem -> Uint16 -> m ()
writeMem16 memReg (MemMain mm) regSeg mem val = do
    offset <- getMemOffset memReg regSeg mem
    liftIO $ pokeByteOff mm offset val

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

findRegSeg :: RegSeg -> Ctx -> RegSeg
findRegSeg defaultRegSeg ctx = getRegSeg (ctxReplaceSeg ctx) 
    where
        getRegSeg Nothing = defaultRegSeg
        getRegSeg (Just s) = s

findRegSegData :: Ctx -> RegSeg
findRegSegData = findRegSeg ds

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
    show (MemBpSi disp) = showMem3 bp si ds disp
    show (MemBpDi disp) = showMem3 bp di ds disp
    show (MemSi disp) = showMem2 si ds disp
    show (MemDi disp) = showMem2 di ds disp
    show (MemBp disp) = showMem2 bp ds disp
    show (MemBx disp) = showMem2 bx ds disp
    show (MemDirect disp) = showMem1 ds disp

-------------------------------------------------------------------------------

calcCFCarry8 :: Uint8 -> Uint8 -> Bool
calcCFCarry8 before after = after < before

calcCFBorrow8 :: Uint8 -> Uint8 -> Bool
calcCFBorrow8 before after = after > before

calcPF8 :: Uint8 -> Bool
calcPF8 = (== 1) . (.&. 0x01) . popCount

calcAFCarry8 :: Uint8 -> Uint8 -> Bool
calcAFCarry8 before after = (after .&. 0x0F) < (before .&. 0x0F)

calcAFBorrow8 :: Uint8 -> Uint8 -> Bool
calcAFBorrow8 before after = (after .&. 0x0F) > (before .&. 0x0F)

calcZF8 :: Uint8 -> Bool
calcZF8 = (==0)

calcSF8 :: Uint8 -> Bool
calcSF8 = (==0x80) . (.&. 0x80) 

calcOF8 :: Uint8 -> Bool
calcOF8 _ = False

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

type FuncImplicit = Ctx -> PrismM
type FuncImm8 = Ctx -> Imm8 -> PrismM

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

-------------------------------------------------------------------------------

-- ctx -> source -> dest -> (ctx, result)
type Func8To8 = Ctx -> Uint8 -> Uint8 -> (Ctx, Uint8)
type Func16To16 = Ctx -> Uint16 -> Uint16 -> (Ctx, Uint16)

instrRegImm8 :: Func8To8 -> FuncRegImm8
instrRegImm8 func ctx reg imm = do
    valReg <- readReg8 memReg reg
    let (ctxNew, valRegNew) = func ctx imm valReg
    writeReg8 memReg reg valRegNew
    return ctxNew
    where
        memReg = ctxReg ctx

instrRegImm16 :: Func16To16 -> FuncRegImm16
instrRegImm16 func ctx reg imm = do
    valReg <- readReg16 memReg reg
    let (ctxNew, valRegNew) = func ctx imm valReg
    writeReg16 memReg reg valRegNew
    return ctxNew
    where
        memReg = ctxReg ctx

instrRegToReg8 :: Func8To8 -> FuncRegReg8
instrRegToReg8 func ctx reg1 reg2 = do
    valReg1 <- readReg8 memReg reg1
    valReg2 <- readReg8 memReg reg2
    let (ctxNew, valRegNew) = func ctx valReg1 valReg2
    writeReg8 memReg reg2 valRegNew
    return ctxNew
    where
        memReg = ctxReg ctx

instrRegToReg16 :: Func16To16 -> FuncRegReg16
instrRegToReg16 func ctx reg1 reg2 = do
    valReg1 <- readReg16 memReg reg1
    valReg2 <- readReg16 memReg reg2
    let (ctxNew, valRegNew) = func ctx valReg1 valReg2
    writeReg16 memReg reg2 valRegNew
    return ctxNew
    where
        memReg = ctxReg ctx
        
instrMemImm8 :: Func8To8 -> FuncMemImm8
instrMemImm8 func ctx mem imm = do
    valMem <- readMem8 memReg memMain regSeg mem
    let (ctxNew, valMemNew) = func ctx imm valMem
    writeMem8 memReg memMain regSeg mem valMemNew
    return ctxNew
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = findRegSegData ctx

instrMemImm16 :: Func16To16 -> FuncMemImm16
instrMemImm16 func ctx mem imm = do
    valMem <- readMem16 memReg memMain regSeg mem
    let (ctxNew, valMemNew) = func ctx imm valMem
    writeMem16 memReg memMain regSeg mem valMemNew
    return ctxNew
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = findRegSegData ctx

instrRegToMem8 :: Func8To8 -> FuncMemReg8
instrRegToMem8 func ctx mem reg = do
    valReg <- readReg8 memReg reg
    valMem <- readMem8 memReg memMain regSeg mem
    let (ctxNew, valMemNew) = func ctx valReg valMem
    writeMem8 memReg memMain regSeg mem valMemNew
    return ctxNew
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = findRegSegData ctx

instrRegToMem16 :: Func16To16 -> FuncMemReg16
instrRegToMem16 func ctx mem reg = do
    valReg <- readReg16 memReg reg
    valMem <- readMem16 memReg memMain regSeg mem
    let (ctxNew, valMemNew) = func ctx valReg valMem
    writeMem16 memReg memMain regSeg mem valMemNew
    return ctxNew
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = findRegSegData ctx

instrMemToReg8 :: Func8To8 -> FuncMemReg8
instrMemToReg8 func ctx mem reg = do
    valReg <- readReg8 memReg reg
    valMem <- readMem8 memReg memMain regSeg mem
    let (ctxNew, valRegNew) = func ctx valMem valReg
    writeReg8 memReg reg valRegNew
    return ctxNew
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = findRegSegData ctx

instrMemToReg16 :: Func16To16 -> FuncMemReg16
instrMemToReg16 func ctx mem reg = do
    valReg <- readReg16 memReg reg
    valMem <- readMem16 memReg memMain regSeg mem
    let (ctxNew, valRegNew) = func ctx valMem valReg
    writeReg16 memReg reg valRegNew
    return ctxNew
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSeg = findRegSegData ctx

instrSegImm16 :: Func16To16 -> FuncSegImm16
instrSegImm16 func ctx regSeg imm = do
    valSeg <- readSeg memReg regSeg
    let (ctxNew, valSegNew) = func ctx imm valSeg
    writeSeg memReg regSeg valSegNew
    return ctxNew
    where
        memReg = ctxReg ctx

instrRegToSeg16 :: Func16To16 -> FuncSegReg16
instrRegToSeg16 func ctx reg regSeg = do
    valReg <- readReg16 memReg reg
    valSeg <- readSeg memReg regSeg
    let (ctxNew, valSegNew) = func ctx valReg valSeg
    writeSeg memReg regSeg valSegNew
    return ctxNew
    where
        memReg = ctxReg ctx

instrSegToReg16 :: Func16To16 -> FuncSegReg16
instrSegToReg16 func ctx reg regSeg = do
    valReg <- readReg16 memReg reg
    valSeg <- readSeg memReg regSeg
    let (ctxNew, valRegNew) = func ctx valSeg valReg
    writeReg16 memReg reg valRegNew
    return ctxNew
    where
        memReg = ctxReg ctx

instrMemToSeg16 :: Func16To16 -> FuncMemSeg16
instrMemToSeg16 func ctx mem regSeg = do
    valSeg <- readSeg memReg regSeg
    valMem <- readMem16 memReg memMain regSegM mem
    let (ctxNew, valSegNew) = func ctx valMem valSeg
    writeSeg memReg regSeg valSegNew
    return ctxNew
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSegM = findRegSegData ctx

instrSegToMem16 :: Func16To16 -> FuncMemSeg16
instrSegToMem16 func ctx mem regSeg = do
    valSeg <- readSeg memReg regSeg
    valMem <- readMem16 memReg memMain regSegM mem
    let (ctxNew, valMemNew) = func ctx valSeg valMem
    writeMem16 memReg memMain regSeg mem valMemNew
    return ctxNew
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
        regSegM = findRegSegData ctx
