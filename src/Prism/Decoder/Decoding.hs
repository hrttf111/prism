module Prism.Decoder.Decoding where

import Data.Maybe (fromJust)
import Data.Either (fromRight)
import Data.Array (Array, array, accumArray, (!), bounds)
import Data.Bits ((.&.), (.|.), shiftR)

import Control.Monad (foldM, liftM)
import Control.Monad.Trans (lift, liftIO, MonadIO)
import Control.Monad.State.Strict

import Foreign.Ptr
import Foreign.Storable (peekByteOff, pokeByteOff)

import Prism.Cpu
import Prism.Instruction

import Prism.Decoder.Common
import Prism.Decoder.Instruction

-------------------------------------------------------------------------------

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

decodeDemux :: Array Uint8 PrismInstrFunc -> PrismInstrFunc
decodeDemux commands instr@(_, b2, _, _, _, _) =
    func instr
    where
        rm = shiftR (b2 .&. 0x38) 3
        func = commands ! rm

-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------

peekInstrBytesM :: Int -> PrismM InstrBytes
peekInstrBytesM offset = do
    mem <- ctxMem <$> get
    peekInstrBytes mem offset

-------------------------------------------------------------------------------
