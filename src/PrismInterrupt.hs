module PrismInterrupt where

import Control.Monad.Trans (lift, liftIO, MonadIO)

import Data.List
import Data.Word
import Data.Maybe (isJust)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import qualified Data.Map.Strict (Map, lookup)

import Foreign.Ptr
import Foreign.Storable (poke, pokeByteOff)

import Prism
import PrismCpu
import PrismPeripheral

addInterruptSingleStep :: PrismInterrupts -> PrismInterrupts
addInterruptSingleStep interrupts = interrupts { intSingleStep = newList, intInterruptUp = True }
    where
       newList = PrismInt 1 : intSingleStep interrupts

addInterruptInt :: PrismInterrupts -> Uint8 -> PrismInterrupts
addInterruptInt interrupts val = interrupts { intListHigh = newList, intInterruptUp = True }
    where
       newList = PrismInt val : intListHigh interrupts

execTF :: Ctx -> Ctx
execTF ctx = if tf then ctx { ctxInterrupts = addInterruptSingleStep $ ctxInterrupts ctx} else ctx
    where
        tf = eflagTF . ctxEFlags $ ctx

interruptActive :: Ctx -> Bool
interruptActive = intInterruptUp . ctxInterrupts

getNextInterruptHigh :: PrismInterrupts -> Maybe (PrismInterrupts, PrismInt)
getNextInterruptHigh interrupts =
    case uncons $ intListHigh interrupts of 
        Just (currentInt, interrupts_) ->
            Just (interrupts { intListHigh = interrupts_}, currentInt)
        Nothing ->
            case uncons $ intSingleStep interrupts of
                Just (currentInt, interrupts_) ->
                    Just (interrupts { intSingleStep = interrupts_}, currentInt)
                Nothing ->
                    Nothing

getNextInterrupt :: (MonadIO m, InterruptDispatcher s) => Bool -> s -> PrismInterrupts -> m (Maybe (PrismInterrupts, PrismInt))
getNextInterrupt if_ dispatcher interrupts =
    let res = getNextInterruptHigh interrupts
        in
    if isJust res then
        return res
        else if if_ && (intIntrOn interrupts) then do
            (_, int) <- liftIO $ ackInterrupt dispatcher
            return $ Just (interrupts {intIntrOn = False}, int)
            else
                return Nothing

processInterrupts :: MonadIO m => Ctx -> m Ctx
processInterrupts ctx = do
    res <- getNextInterrupt (eflagIF . ctxEFlags $ ctx) (ctxIO ctx) (ctxInterrupts ctx)
    case res of
        Just (newInterrupts, (PrismInt val)) -> do
            (ipVal, csVal) <- readMemDirect32 (ctxMem ctx) (4 * (fromIntegral val))
            saveInterruptCtx ctx
            let newCtx = clearIntFlags $ ctx { ctxInterrupts = newInterrupts }
            jmpInterrupt newCtx ipVal csVal
        Nothing -> return $ ctx { ctxInterrupts = (ctxInterrupts $ ctx) { intInterruptUp = False } }

-------------------------------------------------------------------------------

type InterruptHandler = Ctx -> Uint8 -> PrismM
type InterruptMap = Data.Map.Strict.Map Uint8 InterruptHandler

emptyInterruptHandler :: InterruptHandler
emptyInterruptHandler ctx _ = return ctx

intInternal :: InterruptMap -> Ctx -> Uint8 -> PrismM
intInternal mp ctx intType = handler ctx intType
    where
        handler = maybe emptyInterruptHandler id $ Data.Map.Strict.lookup intType mp

writeHandler :: MonadIO m => Ptr Word8 -> Uint8 -> Int -> m ()
writeHandler ptr int addr = liftIO $ (
    pokeByteOff ptr addr (0xF1 :: Word8) >>
    pokeByteOff ptr (addr+1) int >>
    pokeByteOff ptr (addr+2) (0xCF :: Word8) >>
    pokeByteOff ptr (addr+3) (0x00 :: Word8) )

writeInternalInterruptHandlers :: MonadIO m => MemMain -> Int -> [Uint8] -> m [(Uint8, Uint32)]
writeInternalInterruptHandlers (MemMain ptr) address intList = do
    mapM_ (\(int, addr) -> writeHandler ptr int addr) addrList
    return $ map (\(int, addr) -> (int, fromIntegral addr)) addrList
    where
        handlerSize = 4 -- 2 bytes 0xF1, 1 byte iret and 1 byte padding
        (addrList, _) = foldl (\(lst, n) int -> ((int, n) : lst, n + 4)) ([], address) intList

setInterruptsToMemory :: MonadIO m => MemMain -> [(Uint8, Uint32)] -> m ()
setInterruptsToMemory (MemMain ptr) intList =
    liftIO $ mapM_ setHandler intList
    where
        setHandler (int, addr) =
            let cs_ = fromIntegral $ shiftR (addr .&. 0xFFFF0000) 4 :: Uint16
                ip_ = fromIntegral $ addr .&. 0x0000FFFF :: Uint16
                ptrIp = plusPtr ptr ((*4) $ fromIntegral int)
                ptrCs = plusPtr ptrIp 2
            in
            poke ptrCs cs_ >> poke ptrIp ip_

-------------------------------------------------------------------------------

clearIntFlags :: Ctx -> Ctx
clearIntFlags ctx = ctx { ctxEFlags = newEFlags }
    where
        newEFlags = EFlags False False (eflagDF $ ctxEFlags ctx)

saveInterruptCtx :: MonadIO m => Ctx -> m Ctx
saveInterruptCtx ctx = do
    pushInt ctx val
    readSeg memReg cs >>= pushInt ctx
    readRegIP memReg >>= pushInt ctx
    return ctx
    where
        val = flagsToVal (ctxFlags ctx) $ eflagsToVal (ctxEFlags ctx) 0
        memReg = ctxReg ctx

loadInterruptCtx :: MonadIO m => Ctx -> m Ctx
loadInterruptCtx ctx = do
    popInt ctx >>= writeRegIP memReg
    popInt ctx >>= writeSeg memReg cs
    val <- popInt ctx
    let flags = valToFlags val
        eflags = valToEFlags val
    return $ ctx { ctxFlags = flags, ctxEFlags = eflags }
    where
        memReg = ctxReg ctx
jmpInterrupt :: MonadIO m => Ctx -> Imm16 -> Imm16 -> m Ctx 
jmpInterrupt ctx ipVal csVal = do
    writeSeg memReg cs csVal
    writeRegIP memReg ipVal
    return ctx
    where
        memReg = ctxReg ctx

pushInt :: MonadIO m => Ctx -> Uint16 -> m Ctx
pushInt ctx val = do
    valSp <- readReg16 memReg sp
    let valNewSp = valSp - 2
    writeReg16 memReg sp valNewSp
    writeMemSp16 memReg memMain val
    return ctx
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx

popInt :: MonadIO m => Ctx -> m Uint16
popInt ctx = do
    val <- readMemSp16 memReg memMain
    valSp <- readReg16 memReg sp
    let valNewSp = valSp + 2
    writeReg16 memReg sp valNewSp
    return val 
    where
        memReg = ctxReg ctx
        memMain = ctxMem ctx
