module Main where

import Data.Bits((.&.), (.|.), shift)

import Prism
import PrismDecoder
import PrismCpu

import Control.Monad.Trans (lift, liftIO, MonadIO)

import Foreign.Storable (peekByteOff, pokeByteOff)
import Foreign.Marshal.Alloc
import Foreign.Ptr

testDecoder = do
    putStrLn "1"
    ptrReg <- callocBytes 64
    ptrMem <- callocBytes 65000
    let ctx1 = Ctx (MemReg ptrReg) (MemMain ptrMem) clearFlags clearEFlags Nothing
    runPrism $ decodeN8Imm8 freg fmem instr ctx1
    where
        rm = (0x80 .|. 0x0 .|. 0x2) :: Uint8
        --rm = (0xE0 .|. 0x0 .|. 0x2) :: Uint8
        instr = (0x80, rm, 0x7F, 0x10, 0x99, 0x00)
        freg ctx reg imm = liftIO $ do
            putStrLn $ show reg
            putStrLn $ show imm
            return $ ctx
        fmem ctx mem imm = liftIO $ do
            putStrLn $ show mem 
            writeMem8 (ctxReg ctx) (ctxMem ctx) ds mem imm
            memVal <- readMem8 (ctxReg ctx) (ctxMem ctx) ds mem
            putStrLn $ "Mem val: " ++ (show memVal)
            putStrLn $ show imm
            return $ ctx

testDecoder1 = do
    putStrLn "Test decoder simple"
    ptrReg <- callocBytes 64
    ptrMem <- callocBytes 65000
    let ctx1 = Ctx (MemReg ptrReg) (MemMain ptrMem) clearFlags clearEFlags Nothing
    runPrism $ decodeList decoder ctx1 instrs >>= (liftIO . putStrLn . show)
    where
        rm = (0x80 .|. 0x0 .|. 0x3) :: Uint8
        --rm = (0xE0 .|. 0x0 .|. 0x4) :: Uint8
        instr = (0x80, rm, 0x7F, 0x10, 0x99, 0x00)
        instrE = (0x81, rm, 0x7F, 0x10, 0x99, 0x00)
        instrs = [instr, instrE, instr]
        decInstr = decodeN8Imm8 freg fmem
        instrP = makeInstructionS 0x80 Nothing decInstr
        decoder = makeDecoder [instrP]
        freg ctx reg imm = liftIO $ do
            putStrLn $ "MOV " ++ (show reg) ++ ", " ++ (show imm)
            return $ ctx
        fmem ctx mem imm = liftIO $ do
            putStrLn $ "MOV " ++ (show mem) ++ ", " ++ (show imm)
            writeMem8 (ctxReg ctx) (ctxMem ctx) ds mem imm
            memVal <- readMem8 (ctxReg ctx) (ctxMem ctx) ds mem
            putStrLn $ "Mem val: " ++ (show memVal)
            writeReg16 (ctxReg ctx) cx 0x9989
            printRegs (ctxReg ctx)
            putStrLn $ show imm
            return $ ctx

main :: IO ()
main = do
    putStrLn "Start"
    testDecoder1
    return ()
