module Prism.Peripherals.Interrupt where

import Data.List (sortOn)
import Data.Word (Word8)
import Data.Bits ((.&.), (.|.), shiftR, shiftL)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Map.Strict (Map, lookup, fromList)

import Foreign.Ptr
import Foreign.Storable (poke, pokeByteOff)

import Prism.Cpu

-------------------------------------------------------------------------------

type InterruptHandler = Uint8 -> PrismM ()
type InterruptHandlerLocation = (PrismInt, InterruptHandler)
type InterruptMap = Data.Map.Strict.Map Uint8 InterruptHandler

intVector :: PrismInt -> Uint8
intVector (PrismInt v) = v

emptyInterruptHandler :: InterruptHandler
emptyInterruptHandler _ = return ()

intInternal :: InterruptMap -> Uint8 -> PrismM ()
intInternal mp intType = handler intType
    where
        handler = maybe emptyInterruptHandler id $ Data.Map.Strict.lookup intType mp

writeHandler :: MonadIO m => Ptr Word8 -> Uint8 -> Int -> m ()
writeHandler ptr int addr = liftIO $ (
    pokeByteOff ptr addr (0xF1 :: Word8) >>
    pokeByteOff ptr (addr+1) int >>
    pokeByteOff ptr (addr+2) (0xCF :: Word8) >>
    pokeByteOff ptr (addr+3) (0x00 :: Word8) )

--Write interrupts handler`s custom ASM instruction to specific memory location and
--return memory offsets for each handler
writeInternalInterruptHandlers :: MonadIO m => MemMain -> Int -> [Uint8] -> m [(Uint8, Uint32)]
writeInternalInterruptHandlers (MemMain ptr) address intList = do
    mapM_ (\(int, addr) -> writeHandler ptr int addr) addrList
    return $ map (\(int, addr) -> (int, fromIntegral addr)) addrList
    where
        handlerSize = 4 -- 2 bytes 0xF1, 1 byte iret and 1 byte padding
        (addrList, _) = foldl (\(lst, n) int -> ((int, n) : lst, n + 4)) ([], address) intList

--Write locations of memory handlers to specific interrupt vectors
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

configureInterrups :: MonadIO m => MemMain 
                                    -> Int
                                    -> [InterruptHandlerLocation]
                                    -> m InterruptMap
configureInterrups mem address handlers = do
    (mapHandlersToInts <$> writeInternalInterruptHandlers mem address intInternalList)
        >>= setInterruptsToMemory mem
    return . Data.Map.Strict.fromList
        $ map (\(i, (_, handler)) -> (i, handler)) indexedHandlers
    where
        --indexed interrupt handlers
        indexedHandlers = zip [1..] $ sortOn (intVector . fst) handlers
        --generated indexes for custom interrupts
        intInternalList = map fst indexedHandlers
        mapHandlersToInts = foldl func []
        func lst (internalInt, addr) =
            lst ++ (map (\k -> (intVector . fst . snd $ k, addr)) $ filter ((==internalInt) . fst) indexedHandlers)

-------------------------------------------------------------------------------
