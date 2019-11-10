{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

module PrismDecoder where

import Data.Bits ((.&.), (.|.), shiftR, shiftL)

import Numeric (showHex)

import Data.Maybe (fromJust)
import Data.Array (Array, array, accumArray, (!), bounds)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad (foldM)

import Data.Either (fromRight)

import Foreign.Ptr
import Foreign.Storable (peekByteOff, pokeByteOff)

import Prism
import PrismCpu

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

decodeReg8 :: Reg8 -> FuncReg8 -> PrismInstrFunc
decodeReg8 reg freg _ ctx =
    freg ctx reg >>= updateIP 1

decodeReg16 :: Reg16 -> FuncReg16 -> PrismInstrFunc
decodeReg16 reg freg _ ctx =
    freg ctx reg >>= updateIP 1

decodeImm8 :: FuncImm8 -> PrismInstrFunc
decodeImm8 freg (_, b2, _, _, _, _) ctx =
    updateIP 2 ctx >>= flip freg (b2 :: Imm8) 

decodeImm16 :: FuncImm16 -> PrismInstrFunc
decodeImm16 freg (_, b2, b3, _, _, _) ctx =
    let imm16 = getImm16 b2 b3
        in
    updateIP 3 ctx >>= flip freg imm16 

decodeImm32 :: FuncImm32 -> PrismInstrFunc
decodeImm32 freg (_, b2, b3, b4, b5, _) ctx =
    let imm1 = getImm16 b2 b3
        imm2 = getImm16 b4 b5
        in
    updateIP 5 ctx >>= \c -> freg c imm1 imm2

decodeAcc8 :: Reg8 -> FuncRegImm8 -> PrismInstrFunc
decodeAcc8 reg freg (_, b2, _, _, _, _) ctx =
    freg ctx reg (b2 :: Imm8) >>= updateIP 2

decodeAcc16 :: Reg16 -> FuncRegImm16 -> PrismInstrFunc
decodeAcc16 reg freg (_, b2, b3, _, _, _) ctx =
    let imm16 = getImm16 b2 b3
        in
    freg ctx reg imm16 >>= updateIP 3

decodeAccReg8 :: Reg8 -> Reg8 -> FuncRegReg8 -> PrismInstrFunc
decodeAccReg8 reg1 reg2 freg _ ctx =
    freg ctx reg1 reg2 >>= updateIP 1 

decodeAccReg16 :: Reg16 -> Reg16 -> FuncRegReg16 -> PrismInstrFunc
decodeAccReg16 reg1 reg2 freg _ ctx =
    freg ctx reg1 reg2 >>= updateIP 1 

decodeAccSeg :: RegSeg -> FuncSegImm16 -> PrismInstrFunc
decodeAccSeg reg freg (_, b2, b3, _, _, _) ctx =
    let imm16 = getImm16 b2 b3
        in
    freg ctx reg imm16 >>= updateIP 3

decodeAccMem8 :: Reg8 -> FuncMemReg8 -> PrismInstrFunc
decodeAccMem8 reg freg (_, b2, b3, _, _, _) ctx =
    let mem = MemDirect $ getDisp16 b2 b3
        in
    freg ctx mem reg >>= updateIP 3

decodeAccMem16 :: Reg16 -> FuncMemReg16 -> PrismInstrFunc
decodeAccMem16 reg freg (_, b2, b3, _, _, _) ctx =
    let mem = MemDirect $ getDisp16 b2 b3
        in
    freg ctx mem reg >>= updateIP 3

decodeN8Imm8 :: FuncRegImm8 -> FuncMemImm8 -> PrismInstrFunc
decodeN8Imm8 freg fmem (b1, b2, b3, b4, b5, _) ctx = 
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        in
    case mod of
        0x00 ->
            let mem = decodeMem rm 0
                imm8 = b3 :: Imm8
                in
            fmem ctx mem imm8 >>= updateIP 3
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem rm disp8
                imm8 = b4 :: Imm8
                in
            fmem ctx mem imm8 >>= updateIP 4
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem rm disp16
                imm8 = b5 :: Imm8
                in
            fmem ctx mem imm8 >>= updateIP 5
        0x03 ->
            let reg = Reg8 rm
                imm8 = b3 :: Imm8
                in
            freg ctx reg imm8 >>= updateIP 3


decodeN16Imm :: FuncRegImm16 -> FuncMemImm16 -> PrismInstrFunc
decodeN16Imm freg fmem (b1, b2, b3, b4, b5, b6) ctx = 
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        in
    case mod of
        0x00 ->
            let mem = decodeMem rm 0
                imm16 = getImm16 b3 b4
                in
            fmem ctx mem imm16 >>= updateIP 4
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem rm disp8
                imm16 = getImm16 b4 b5
                in
            fmem ctx mem imm16 >>= updateIP 5
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem rm disp16
                imm16 = getImm16 b5 b6
                in
            fmem ctx mem imm16 >>= updateIP 6
        0x03 ->
            let reg = Reg16 rm
                imm16 = getImm16 b3 b4
                in
            freg ctx reg imm16 >>= updateIP 4


decodeN16Imm8 :: FuncRegImm16 -> FuncMemImm16 -> PrismInstrFunc
decodeN16Imm8 freg fmem (b1, b2, b3, b4, b5, _) ctx = 
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        in
    case mod of
        0x00 ->
            let mem = decodeMem rm 0
                imm16 = signExterndWord $ getImm8 b3
                in
            fmem ctx mem imm16 >>= updateIP 3
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem rm disp8
                imm16 = signExterndWord $ getImm8 b4
                in
            fmem ctx mem imm16 >>= updateIP 4
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem rm disp16
                imm16 = signExterndWord $ getImm8 b5
                in
            fmem ctx mem imm16 >>= updateIP 5
        0x03 ->
            let reg = Reg16 rm
                imm16 = signExterndWord $ getImm8 b3
                in
            freg ctx reg imm16 >>= updateIP 3


decodeN16 :: FuncReg16 -> FuncMem -> PrismInstrFunc
decodeN16 freg fmem (b1, b2, b3, b4, _, _) ctx =
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        in
    case mod of
        0x00 ->
            let mem = decodeMem rm 0
                in
            fmem ctx mem >>= updateIP 2
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem rm disp8
                in
            fmem ctx mem >>= updateIP 3
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem rm disp16
                in
            fmem ctx mem >>= updateIP 4
        0x03 ->
            let reg = Reg16 rm
                in
            freg ctx reg >>= updateIP 2


decodeN8 :: FuncReg8 -> FuncMem -> PrismInstrFunc
decodeN8 freg fmem (b1, b2, b3, b4, _, _) ctx =
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        in
    case mod of
        0x00 ->
            let mem = decodeMem rm 0
                in
            fmem ctx mem >>= updateIP 2
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem rm disp8
                in
            fmem ctx mem >>= updateIP 3
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem rm disp16
                in
            fmem ctx mem >>= updateIP 4
        0x03 ->
            let reg = Reg8 rm
                in
            freg ctx reg >>= updateIP 2


decodeRm8 :: FuncRegReg8 -> FuncMemReg8 -> PrismInstrFunc
decodeRm8 freg fmem (b1, b2, b3, b4, _, _) ctx =
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        reg = Reg8 $ shiftR (modrm .&. 0x38) 3
        in
    case mod of
        0x00 ->
            let mem = decodeMem rm 0
                in
            fmem ctx mem reg >>= updateIP 2
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem rm disp8
                in
            fmem ctx mem reg >>= updateIP 3
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem rm disp16
                in
            fmem ctx mem reg >>= updateIP 4
        0x03 ->
            let reg2 = Reg8 rm
                in
            freg ctx reg2 reg >>= updateIP 2


decodeRm16 :: FuncRegReg16 -> FuncMemReg16 -> PrismInstrFunc
decodeRm16 freg fmem (b1, b2, b3, b4, _, _) ctx =
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        reg = Reg16 $ shiftR (modrm .&. 0x38) 3
        in
    case mod of
        0x00 ->
            let mem = decodeMem rm 0
                in
            fmem ctx mem reg >>= updateIP 2
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem rm disp8
                in
            fmem ctx mem reg >>= updateIP 3
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem rm disp16
                in
            fmem ctx mem reg >>= updateIP 4
        0x03 ->
            let reg2 = Reg16 rm
                in
            freg ctx reg2 reg >>= updateIP 2


decodeRmS16 :: FuncSegReg16 -> FuncMemSeg16 -> PrismInstrFunc
decodeRmS16 freg fmem (b1, b2, b3, b4, _, _) ctx =
    let modrm = b2
        mod = shiftR (modrm .&. 0xE0) 6
        rm = modrm .&. 0x07
        reg = RegSeg $ shiftR (modrm .&. 0x38) 3
        in
    case mod of
        0x00 ->
            let mem = decodeMem rm 0
                in
            fmem ctx mem reg >>= updateIP 2
        0x01 -> 
            let disp8 = getDisp8 b3
                mem = decodeMem rm disp8
                in
            fmem ctx mem reg >>= updateIP 3
        0x02 ->
            let disp16 = getDisp16 b3 b4 
                mem = decodeMem rm disp16
                in
            fmem ctx mem reg >>= updateIP 4
        0x03 ->
            let reg2 = Reg16 rm
                in
            freg ctx reg2 reg >>= updateIP 2


decodeList :: PrismDecoder -> Ctx -> [InstrBytes] -> PrismCtx IO Ctx
decodeList _ ctx [] = return ctx
decodeList dec ctx (x:xs) = do
    ctx1 <- instr x ctx
    decodeList dec ctx1 xs
    where
        (b1, _, _, _, _, _) = x
        instr = instrFunc $ (decInstr dec) ! b1

decodeMemIp :: PrismDecoder -> Int -> Ctx -> PrismCtx IO Ctx
decodeMemIp dec len ctx = do
    ip <- readRegIP memReg 
    offset <- getInstrAddress memReg cs ip
    if (fromIntegral offset) >= len then return ctx
    else do
        instr <- peekInstrBytes (ctxMem ctx) offset
        let (b1, _, _, _, _, _) = instr
            func = instrFunc $ (decInstr dec) ! b1
        liftIO $ putStrLn (showHex b1 "")
        func instr ctx >>= decodeMemIp dec len
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
