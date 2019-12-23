{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE TypeSynonymInstances #-}

module PrismDecoder where

import Data.Bits ((.&.), (.|.), shiftR, shiftL)

import Numeric (showHex)

import Data.Maybe (fromJust)
import Data.Array (Array, array, accumArray, (!), bounds)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad (foldM, liftM)

import Data.Either (fromRight)

import Foreign.Ptr
import Foreign.Storable (peekByteOff, pokeByteOff)

import Prism
import PrismCpu
import PrismInterrupt
import PrismCommand

-------------------------------------------------------------------------------

type PrismInstrFunc = InstrBytes -> Ctx -> PrismCtx IO Ctx

data PrismInstruction = PrismInstruction {
        instrOpcode :: Uint8,
        instrRm :: Maybe Uint8,
        instrFunc :: PrismInstrFunc,
        instrDemux :: Array Uint8 PrismInstrFunc
    }

instance Show PrismInstruction where
    show (PrismInstruction opcode rm _ arr) = "PrismInstruction "
        ++ (show opcode) 
        ++ " " ++ (show rm)
        ++ " " ++ (show $ bounds arr)

data PrismDecoder = PrismDecoder {
        decInstr :: Array Uint8 PrismInstruction
    }

makeDecoder :: [PrismInstruction] -> PrismDecoder
makeDecoder list =
    let arr = accumArray acc instrEmpty (0, 255) listF
        acc _ i = i
        instrEmpty = makeInstructionS 0 Nothing decodeEmpty
        listF = map (\i -> (instrOpcode i, i)) list
        in
    PrismDecoder arr

makeInstructionS :: Uint8 -> Maybe Uint8 -> PrismInstrFunc -> PrismInstruction
makeInstructionS opcode rm func = PrismInstruction opcode rm func $ emptyArray

emptyArray :: Array Uint8 PrismInstrFunc
emptyArray = array (0, 0) []

mergeInstruction :: Uint8 -> [PrismInstruction] -> PrismInstruction
mergeInstruction opcode [] = 
    PrismInstruction opcode Nothing decodeEmpty $ emptyArray
mergeInstruction _ [instr] = instr
mergeInstruction opcode funcs = 
    let acc _ i = i
        arr = accumArray acc decodeEmpty (0, 7) listF
        listF = map (\i -> (fromJust $ instrRm i, instrFunc i)) funcs
        func = decodeDemux arr
        in
    PrismInstruction opcode Nothing func arr 

type ListResult = Either String InstructionList
type InstructionList = [(Uint8, [PrismInstruction])]

addInstrList :: PrismInstruction -> InstructionList -> ListResult
addInstrList instr@(PrismInstruction opcode _ _ _) [] = Right [(opcode, [instr])]
addInstrList instr@(PrismInstruction opcode rm _ _) ((i, l):xs) | opcode == i = 
    case rm of
        Nothing ->
            Left $ "Cannot add wildcard, item already exists "  ++ (show instr)
        Just r | isUnique r l ->
            Right ((opcode, (instr:l)):xs)
        Just _ ->
            Left $ "Not unique rm " ++ (show instr)
    where
        isUnique _ [] = True
        isUnique a ((PrismInstruction _ Nothing _ _):_) = False
        isUnique a ((PrismInstruction _ (Just x) _ _):xs) | a == x = False
        isUnique a (_:xs) = isUnique a xs
addInstrList a (a1:xs) = (a1:) <$> addInstrList a xs

-------------------------------------------------------------------------------

decodeEmpty :: PrismInstrFunc
decodeEmpty _ ctx = liftIO $ putStrLn "Empty" >> (updateIP 1 ctx)

decodeDemux :: Array Uint8 PrismInstrFunc -> PrismInstrFunc
decodeDemux commands instr@(_, b2, _, _, _, _) ctx =
    func instr ctx
    where
        rm = shiftR (b2 .&. 0x38) 3
        func = commands ! rm

-------------------------------------------------------------------------------

decodeImplicit :: FuncImplicit -> PrismInstrFunc
decodeImplicit freg _ ctx =
    updateIP 1 ctx >>= freg

decodeImm1 :: (ImmDecoder b, OperandVal b) =>
    FuncImm1 b -> PrismInstrFunc
decodeImm1 func (_, b2, b3, _, _, _) ctx =
    let imm = decodeImm b2 b3
        in
    updateIP (1 + immLength imm) ctx >>= flip func imm

{-# SPECIALISE decodeImm1 :: FuncImm1 Imm8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeImm1 :: FuncImm1 Imm16 -> PrismInstrFunc #-}

decodeImm8 :: FuncImm1 Imm8 -> PrismInstrFunc
decodeImm8 = decodeImm1

decodeImm16 :: FuncImm1 Imm16 -> PrismInstrFunc
decodeImm16 = decodeImm1

decodeImm32 :: FuncImm2 Imm16 -> PrismInstrFunc
decodeImm32 freg (_, b2, b3, b4, b5, _) ctx =
    let imm1 = getImm16 b2 b3
        imm2 = getImm16 b4 b5
        in
    updateIP 5 ctx >>= \c -> freg c imm1 imm2

-------------------------------------------------------------------------------

decodeStR :: (OperandVal b, OperandReg a b) =>
    a -> FuncO1M a -> PrismInstrFunc 
decodeStR reg func _ ctx =
    func ctx reg >>= updateIP 1

{-# SPECIALISE decodeStR :: Reg8 -> FuncO1M Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStR :: Reg16 -> FuncO1M Reg16 -> PrismInstrFunc #-}

--Decode IMM and execute function with predefined REG
decodeStRI :: (ImmDecoder b, OperandVal b, OperandReg a b) =>
    a -> FuncOI1M a b -> PrismInstrFunc
decodeStRI reg func (_, b2, b3, _, _, _) ctx =
    let imm = decodeImm b2 b3 
        in
    func ctx reg imm >>= updateIP (1 + immLength imm)

{-# SPECIALISE decodeStRI :: Reg8 -> FuncOI1M Reg8 Uint8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRI :: Reg16 -> FuncOI1M Reg16 Uint16 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRI :: RegSeg -> FuncOI1M RegSeg Uint16 -> PrismInstrFunc #-}

decodeStRR :: (OperandVal b, OperandReg a b) =>
    a -> a -> FuncO2M a a -> PrismInstrFunc
decodeStRR reg1 reg2 func _ ctx =
    func ctx reg1 reg2 >>= updateIP 1

{-# SPECIALISE decodeStRR :: Reg8 -> Reg8 -> FuncO2M Reg8 Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRR :: Reg16 -> Reg16 -> FuncO2M Reg16 Reg16 -> PrismInstrFunc #-}

decodeStRM :: (OperandVal b, OperandMem a1 b, OperandReg a2 b) =>
    a2 -> FuncO2M a1 a2 -> PrismInstrFunc
decodeStRM reg func (_, b2, b3, _, _, _) ctx =
    let mem = decodeMemDirect $ getDisp16 b2 b3
        in
    func ctx mem reg >>= updateIP 3

{-# SPECIALISE decodeStRM :: Reg8 -> FuncO2M Mem8 Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeStRM :: Reg16 -> FuncO2M Mem16 Reg16 -> PrismInstrFunc #-}

decodeStRM8 :: Reg8 -> FuncO2M Mem8 Reg8 -> PrismInstrFunc
decodeStRM8 = decodeStRM

decodeStRM16 :: Reg16 -> FuncO2M Mem16 Reg16 -> PrismInstrFunc
decodeStRM16 = decodeStRM

-------------------------------------------------------------------------------

decodeNI :: (ImmDecoder b, OperandVal b, OperandReg a1 b, OperandMem a2 b) => 
    FuncOI1M a1 b -> FuncOI1M a2 b -> PrismInstrFunc
decodeNI freg fmem (b1, b2, b3, b4, b5, b6) ctx = 
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        in
    case mod of
        0x00 ->
            case rm of
                0x06 ->
                    let disp16 = getDisp16 b3 b4
                        imm = decodeImm b5 b6
                        mem = decodeMemDirect disp16
                        in
                    fmem ctx mem imm >>= updateIP (4 + (immLength imm))
                _ ->
                    let mem = decodeMem1 rm 0
                        imm = decodeImm b3 b4
                        in
                    fmem ctx mem imm >>= updateIP (2 + (immLength imm))
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem1 rm disp8
                imm = decodeImm b4 b5
                in
            fmem ctx mem imm >>= updateIP (3 + (immLength imm))
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem1 rm disp16
                imm = decodeImm b5 b6
                in
            fmem ctx mem imm >>= updateIP (4 + (immLength imm))
        0x03 ->
            let reg = decodeReg rm
                imm = decodeImm b3 b4
                in
            freg ctx reg imm >>= updateIP (2 + (immLength imm))

{-# SPECIALISE decodeNI :: FuncOI1M Reg8 Uint8 -> FuncOI1M Mem8 Uint8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeNI :: FuncOI1M Reg16 Uint16 -> FuncOI1M Mem16 Uint16 -> PrismInstrFunc #-}

decodeNI8 :: FuncOI1M Reg8 Uint8 -> FuncOI1M Mem8 Uint8 -> PrismInstrFunc
decodeNI8 = decodeNI

decodeNI16 :: FuncOI1M Reg16 Uint16 -> FuncOI1M Mem16 Uint16 -> PrismInstrFunc
decodeNI16 = decodeNI

-------------------------------------------------------------------------------

decodeNC :: (ImmDecoder b, OperandVal b, OperandReg a1 b, OperandMem a2 b) => 
    FuncOI1M a1 b -> FuncOI1M a2 b -> PrismInstrFunc
decodeNC freg fmem bytes ctx = 
    decodeNI8 freg8 fmem8 bytes ctx
    where
        freg8 ctx reg imm8 = freg ctx (convertReg reg) $ signExterndWordN imm8
        fmem8 ctx mem imm8 = fmem ctx (convertMem mem) $ signExterndWordN imm8

{-# SPECIALISE decodeNC :: FuncOI1M Reg16 Uint16 -> FuncOI1M Mem16 Uint16 -> PrismInstrFunc #-}

decodeNC16 :: FuncOI1M Reg16 Uint16 -> FuncOI1M Mem16 Uint16 -> PrismInstrFunc
decodeNC16 = decodeNC

-------------------------------------------------------------------------------

decodeRM :: (OperandVal b, OperandReg a1 b, OperandReg a2 b, OperandMem a3 b) => 
    FuncO2M a2 a1 -> FuncO2M a3 a1 -> PrismInstrFunc
decodeRM freg fmem (b1, b2, b3, b4, _, _) ctx =
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        reg = decodeReg $ shiftR (modrm .&. 0x38) 3
        in
    case mod of
        0x00 ->
            case rm of
                0x06 ->
                    let disp16 = getDisp16 b3 b4
                        mem = decodeMemDirect disp16
                        in
                    fmem ctx mem reg >>= updateIP 4
                _ ->
                    let mem = decodeMem1 rm 0
                        in
                    fmem ctx mem reg >>= updateIP 2
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem1 rm disp8
                in
            fmem ctx mem reg >>= updateIP 3
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem1 rm disp16
                in
            fmem ctx mem reg >>= updateIP 4
        0x03 ->
            let reg2 = decodeReg rm
                in
            freg ctx reg2 reg >>= updateIP 2

{-# SPECIALISE decodeRM :: FuncO2M Reg8 Reg8 -> FuncO2M Mem8 Reg8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeRM :: FuncO2M Reg16 Reg16 -> FuncO2M Mem16 Reg16 -> PrismInstrFunc #-}
{-# SPECIALISE decodeRM :: FuncO2M Reg16 RegSeg -> FuncO2M Mem16 RegSeg -> PrismInstrFunc #-}

decodeRM8 :: FuncO2M Reg8 Reg8 -> FuncO2M Mem8 Reg8 -> PrismInstrFunc
decodeRM8 = decodeRM

decodeRM16 :: FuncO2M Reg16 Reg16 -> FuncO2M Mem16 Reg16 -> PrismInstrFunc
decodeRM16 = decodeRM

decodeRMS16 :: FuncO2M Reg16 RegSeg -> FuncO2M Mem16 RegSeg -> PrismInstrFunc
decodeRMS16 = decodeRM

-------------------------------------------------------------------------------

decodeN :: (OperandVal b, OperandReg a1 b, OperandMem a2 b) =>
    FuncO1M a1 -> FuncO1M a2 -> PrismInstrFunc
decodeN freg fmem (b1, b2, b3, b4, _, _) ctx =
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        in
    case mod of
        0x00 ->
            case rm of
                0x06 ->
                    let disp16 = getDisp16 b3 b4
                        mem = decodeMemDirect disp16
                        in
                    fmem ctx mem >>= updateIP 4
                _ ->
                    let mem = decodeMem1 rm 0
                        in
                    fmem ctx mem >>= updateIP 2
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem1 rm disp8
                in
            fmem ctx mem >>= updateIP 3
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem1 rm disp16
                in
            fmem ctx mem >>= updateIP 4
        0x03 ->
            let reg = decodeReg rm
                in
            freg ctx reg >>= updateIP 2

{-# SPECIALISE decodeN :: FuncO1M Reg8 -> FuncO1M Mem8 -> PrismInstrFunc #-}
{-# SPECIALISE decodeN :: FuncO1M Reg16 -> FuncO1M Mem16 -> PrismInstrFunc #-}

decodeN8 :: FuncO1M Reg8 -> FuncO1M Mem8 -> PrismInstrFunc
decodeN8 = decodeN

decodeN16 :: FuncO1M Reg16 -> FuncO1M Mem16 -> PrismInstrFunc
decodeN16 = decodeN

-------------------------------------------------------------------------------

decodeList :: PrismDecoder -> Ctx -> [InstrBytes] -> PrismCtx IO Ctx
decodeList _ ctx [] = return ctx
decodeList dec ctx (x:xs) = do
    ctx1 <- instr x ctx
    decodeList dec ctx1 xs
    where
        (b1, _, _, _, _, _) = x
        instr = instrFunc $ (decInstr dec) ! b1

decodeExecOne :: PrismDecoder -> Ctx -> PrismCtx IO Ctx 
decodeExecOne dec ctx = do
    offset <- getInstrAddress memReg cs =<< readRegIP memReg
    instr <- peekInstrBytes (ctxMem ctx) offset
    let (b1, _, _, _, _, _) = instr
        func = instrFunc $ (decInstr dec) ! b1
    liftIO $ putStrLn (showHex b1 "_One")
    func instr ctx
    where
        memReg = ctxReg ctx

decodeMemIp :: PrismDecoder -> Int -> Ctx -> PrismCtx IO Ctx
decodeMemIp dec len ctx = do
    offset <- getInstrAddress memReg cs =<< readRegIP memReg
    if (fromIntegral offset) >= len then return ctx
    else do
        if interruptActive ctx then processInterrupts ctx >>= decodeMemIp dec len
        else do
            instr <- peekInstrBytes (ctxMem ctx) offset
            let (b1, _, _, _, _, _) = instr
                func = instrFunc $ (decInstr dec) ! b1
            liftIO $ putStrLn (showHex b1 "")
            execTF <$> func instr ctx >>= decodeMemIp dec len
    where
        memReg = ctxReg ctx

decodeHalt :: PrismDecoder -> Ctx -> PrismCtx IO Ctx
decodeHalt dec ctx = do
    if ctxStop ctx then return ctx
    else do
        if interruptActive ctx then processInterrupts ctx >>= decodeHalt dec
        else do
            offset <- getInstrAddress memReg cs =<< readRegIP memReg
            instr <- peekInstrBytes (ctxMem ctx) offset
            let (b1, _, _, _, _, _) = instr
                func = instrFunc $ (decInstr dec) ! b1
            liftIO $ putStrLn (showHex b1 "")
            execTF <$> func instr ctx >>= decodeHalt dec 
    where
        memReg = ctxReg ctx

decodeHaltCpu :: PrismDecoder -> PrismComm -> Ctx -> PrismCtx IO Ctx
decodeHaltCpu dec comm ctx = do
    offset <- getInstrAddress memReg cs =<< readRegIP memReg
    m <- processPrismCommand comm ctx offset
    case m of
        Just (comm_, ctx_) -> decodeHaltCpu dec comm_ ctx_
        Nothing -> 
            if ctxStop ctx then return ctx
            else do
                if interruptActive ctx then processInterrupts ctx >>= decodeHaltCpu dec comm
                else do
                    instr <- peekInstrBytes (ctxMem ctx) offset
                    let (b1, _, _, _, _, _) = instr
                        func = instrFunc $ (decInstr dec) ! b1
                    liftIO $ putStrLn (showHex b1 "")
                    execTF <$> func instr ctx >>= decodeHaltCpu dec comm
    where
        memReg = ctxReg ctx

pokeTuple6 :: MonadIO m => Ptr Uint8 -> Int -> (Uint8, Uint8, Uint8, Uint8, Uint8, Uint8) -> m (Ptr Uint8)
pokeTuple6 ptr offset (b1, b2, b3, b4, b5, b6) = liftIO $ do
    pokeByteOff ptr offset b1
    pokeByteOff ptr (offset + 1) b2
    pokeByteOff ptr (offset + 2) b3
    pokeByteOff ptr (offset + 3) b4
    pokeByteOff ptr (offset + 4) b5
    pokeByteOff ptr (offset + 5) b6
    return ptr

peekTuple6 :: MonadIO m => Ptr Uint8 -> Int -> m (Uint8, Uint8, Uint8, Uint8, Uint8, Uint8)
peekTuple6 ptr offset = liftIO $ do
    b1 <- peekByteOff ptr offset
    b2 <- peekByteOff ptr (offset + 1)
    b3 <- peekByteOff ptr (offset + 2)
    b4 <- peekByteOff ptr (offset + 3)
    b5 <- peekByteOff ptr (offset + 4)
    b6 <- peekByteOff ptr (offset + 5)
    return (b1, b2, b3, b4, b5, b6)

peekFirstByte :: MonadIO m => MemMain -> Int -> m Uint8
peekFirstByte (MemMain ptr) offset = liftIO $ peekByteOff ptr offset

pokeInstrBytes :: MonadIO m => MemMain -> Int -> InstrBytes -> m (Ptr Uint8)
pokeInstrBytes (MemMain ptr) offset instr = pokeTuple6 ptr offset instr

peekInstrBytes :: MonadIO m => MemMain -> Int -> m InstrBytes
peekInstrBytes (MemMain ptr) offset = peekTuple6 ptr offset

makeDecoderList :: [PrismInstruction] -> PrismDecoder
makeDecoderList instrList = fromRight emptyDecoder $ makeDecoder <$> mergedInstr
    where
        listResult = foldM (flip addInstrList) [] instrList
        mergedInstr :: Either String [PrismInstruction]
        mergedInstr = map (uncurry mergeInstruction) <$> listResult
        emptyDecoder = makeDecoder []

internalInstrList mp = [
        makeInstructionS 0xF1 Nothing (decodeImm8 $ intInternal mp)
    ]
