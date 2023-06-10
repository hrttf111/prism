module Prism.Instructions.Control where

import Control.Monad.Trans (lift, liftIO, MonadIO)

import Data.Bits (shiftL)
import Prism.Cpu
import Prism.InstructionM

-------------------------------------------------------------------------------

jmpShort :: (CpuMonad m) => FuncImm1 Imm8 m
jmpShort val =
    jmpNear $ signExtendWord val

jmpNear :: (CpuMonad m) => FuncImm1 Imm16 m
jmpNear val =
    ((val+) <$> readOp ip) >>= writeOp ip

jmpIntra :: (CpuMonad m, MonadIO m) => FuncImm1 Imm16 m
jmpIntra ipVal = do
    --liftIO $ putStrLn $ "Intra ipVal = " ++ (show ipVal)
    writeOp ip ipVal

jmpInter :: (CpuMonad m, MonadIO m) => FuncImm2 Imm16 m
jmpInter ipVal csVal = do
    offsetFrom <- regsToOffset <$> readOp cs <*> readOp ip
    let offsetTo = regsToOffset csVal ipVal
    liftIO $ putStrLn $ "FAR ipVal = "
                         ++ (show ipVal)
                         ++ "; csVal = "
                         ++ (show csVal)
                         ++ "; from = " ++ (show offsetFrom)
                         ++ "; to = " ++ (show offsetTo)
    writeOp cs csVal >> writeOp ip ipVal

instrJMem32 :: (CpuMonad m) => FuncImm2 Imm16 m -> FuncO1M MemSeg16 m
instrJMem32 func mem = do
    val1 <- readOp mem
    val2 <- readOp $ mapMem (+2) mem
    func val1 val2

-------------------------------------------------------------------------------

regsToOffset :: Uint16 -> Uint16 -> Int
regsToOffset csVal ipVal = ((shiftL (fromIntegral csVal) 4) + (fromIntegral ipVal)) :: Int

callNear :: (CpuMonad m, MonadIO m) => FuncImm1 Imm16 m
callNear val = do
    ipVal <- readOp ip
    pushV ipVal
    writeOp ip (ipVal + val)

callIntra :: (CpuMonad m, MonadIO m) => FuncImm1 Imm16 m
callIntra val = do
    ipVal <- readOp ip
    --liftIO $ putStrLn $ "Call Intra ipVal = " ++ (show ipVal)
    pushV ipVal
    writeOp ip val

callInter :: (CpuMonad m, MonadIO m) => FuncImm2 Imm16 m
callInter ipVal csVal = do
    ipValOld <- readOp ip
    csValOld <- readOp cs
    pushV csValOld
    pushV ipValOld
    offsetFrom <- regsToOffset <$> readOp cs <*> readOp ip
    let offsetTo = regsToOffset csVal ipVal
    liftIO $ putStrLn $ "CALL FAR ipVal = "
                         ++ (show ipVal)
                         ++ "; csVal = "
                         ++ (show csVal)
                         ++ "; from = " ++ (show offsetFrom)
                         ++ "; to = " ++ (show offsetTo)
    writeOp cs csVal
    writeOp ip ipVal

retIntra :: (CpuMonad m, MonadIO m) => FuncImm1 Imm16 m
retIntra val = do
    ipVal <- popV
    --liftIO $ putStrLn $ "Ret Intra ipVal = " ++ (show ipVal)
    writeOp ip ipVal
    if val /= 0 then do
        spOld <- readOp sp
        writeOp sp $ spOld + val
        else return ()

retInter :: (CpuMonad m, MonadIO m) => FuncImm1 Imm16 m
retInter val = do
    ipVal <- popV
    csVal <- popV
    offsetFrom <- regsToOffset <$> readOp cs <*> readOp ip
    let offsetTo = regsToOffset csVal ipVal
    liftIO $ putStrLn $ "RET FAR ipVal = "
                         ++ (show ipVal)
                         ++ "; csVal = "
                         ++ (show csVal)
                         ++ "; from = " ++ (show offsetFrom)
                         ++ "; to = " ++ (show offsetTo)
    writeOp ip (ipVal :: Uint16)
    writeOp cs (csVal :: Uint16)
    if val /= 0 then do
        spOld <- readOp sp
        writeOp sp $ spOld + val
        else return ()

-------------------------------------------------------------------------------

cfIsSet = flagCF
cfIsClear = not . cfIsSet

ofIsSet = flagOF
ofIsClear = not . ofIsSet

zfIsSet = flagZF
zfIsClear = not . zfIsSet

sfIsSet = flagSF
sfIsClear = not . sfIsSet

pfIsSet = flagPF
pfIsClear = not . pfIsSet

sfXorOf flags = (/=) (flagSF flags) (flagOF flags)

xxxJmpShort :: (CpuMonad m) => (Flags -> Bool) -> FuncImm1 Imm8 m
xxxJmpShort func val = do
    flags <- getFlags
    if func flags then
        jmpShort val
        else return ()

ja :: (CpuMonad m) => FuncImm1 Imm8 m
ja = xxxJmpShort (\f -> cfIsClear f && zfIsClear f)

jnbe :: (CpuMonad m) => FuncImm1 Imm8 m
jnbe = ja

jae :: (CpuMonad m) => FuncImm1 Imm8 m
jae = jnc

jnb :: (CpuMonad m) => FuncImm1 Imm8 m
jnb = jnc

jb :: (CpuMonad m) => FuncImm1 Imm8 m
jb = jc

jnae :: (CpuMonad m) => FuncImm1 Imm8 m
jnae = jc

jbe :: (CpuMonad m) => FuncImm1 Imm8 m
jbe = xxxJmpShort (\f -> cfIsSet f || zfIsSet f)

jna :: (CpuMonad m) => FuncImm1 Imm8 m
jna = jbe

jc :: (CpuMonad m) => FuncImm1 Imm8 m
jc = xxxJmpShort cfIsSet

je :: (CpuMonad m) => FuncImm1 Imm8 m
je = xxxJmpShort zfIsSet

jz :: (CpuMonad m) => FuncImm1 Imm8 m
jz = je

jg :: (CpuMonad m) => FuncImm1 Imm8 m
jg = xxxJmpShort (\f -> not (sfXorOf f || zfIsSet f))

jnle :: (CpuMonad m) => FuncImm1 Imm8 m
jnle = jg

jge :: (CpuMonad m) => FuncImm1 Imm8 m
jge = xxxJmpShort (not . sfXorOf)

jnl :: (CpuMonad m) => FuncImm1 Imm8 m
jnl = jge

jl :: (CpuMonad m) => FuncImm1 Imm8 m
jl = xxxJmpShort sfXorOf

jnge :: (CpuMonad m) => FuncImm1 Imm8 m
jnge = jl

jle :: (CpuMonad m) => FuncImm1 Imm8 m
jle = xxxJmpShort (\f -> sfXorOf f || zfIsSet f)

jng :: (CpuMonad m) => FuncImm1 Imm8 m
jng = jle

jnc :: (CpuMonad m) => FuncImm1 Imm8 m
jnc = xxxJmpShort cfIsClear

jne :: (CpuMonad m) => FuncImm1 Imm8 m
jne = xxxJmpShort zfIsClear

jnz :: (CpuMonad m) => FuncImm1 Imm8 m
jnz = jne

jno :: (CpuMonad m) => FuncImm1 Imm8 m
jno = xxxJmpShort ofIsClear

jnp :: (CpuMonad m) => FuncImm1 Imm8 m
jnp = xxxJmpShort pfIsClear

jpo :: (CpuMonad m) => FuncImm1 Imm8 m
jpo = jnp

jns :: (CpuMonad m) => FuncImm1 Imm8 m
jns = xxxJmpShort sfIsClear

jo :: (CpuMonad m) => FuncImm1 Imm8 m
jo = xxxJmpShort ofIsSet

jp :: (CpuMonad m) => FuncImm1 Imm8 m
jp = xxxJmpShort pfIsSet

jpe :: (CpuMonad m) => FuncImm1 Imm8 m
jpe = jp

js :: (CpuMonad m) => FuncImm1 Imm8 m
js = xxxJmpShort sfIsSet

-------------------------------------------------------------------------------

loop :: (CpuMonad m) => FuncImm1 Imm8 m
loop val = do
    regVal <- readOp cx
    let newRegVal = regVal - 1
    writeOp cx newRegVal
    if newRegVal /= 0 then
        jmpShort val
        else return ()

loopZ :: (CpuMonad m) => FuncImm1 Imm8 m
loopZ val = do
    regVal <- readOp cx
    let newRegVal = regVal - 1
    writeOp cx newRegVal
    flags <- getFlags
    if newRegVal /= 0 && zfIsSet flags then
        jmpShort val
        else return ()

loopE :: (CpuMonad m) => FuncImm1 Imm8 m
loopE = loopZ

loopNZ :: (CpuMonad m) => FuncImm1 Imm8 m
loopNZ val = do
    regVal <- readOp cx
    let newRegVal = regVal - 1
    writeOp cx newRegVal
    flags <- getFlags
    if newRegVal /= 0 && zfIsClear flags then
        jmpShort val
        else return ()

loopNE :: (CpuMonad m) => FuncImm1 Imm8 m
loopNE = loopNZ

jcxz :: (CpuMonad m) => FuncImm1 Imm8 m
jcxz val = do
    regVal <- readOp cx
    if regVal == 0 then
        jmpShort val
        else return ()

-------------------------------------------------------------------------------
