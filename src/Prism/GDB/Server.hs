{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Prism.GDB.Server where

import Control.Monad.Trans (MonadIO, liftIO)
import qualified Control.Monad.State.Class as SC
import Control.Monad.Logger.CallStack

import Control.Exception.Lifted (bracket)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (maybe, fromJust)
import Data.Word (Word32, Word16, Word8)
import Data.Bits (shiftL)
import Numeric (showHex)

import Foreign.Marshal.Array
import Foreign.Ptr

import Network.Socket(Socket, close, bind, accept, listen, socket, defaultProtocol, Family(..), connect, tupleToHostAddress, SockAddr(..), SocketType(..))
import qualified Net.IPv4(toOctets, decodeString)
import qualified Network.Socket.ByteString as SocketB
import System.Timeout

import Text.Parsec
import qualified Data.Text as T

import GHC.Stack (HasCallStack, callStack)

import Prism.GDB.Types
import Prism.GDB.Protocol

import Prism.Cpu
import Prism.Command

-------------------------------------------------------------------------------

-- $packet-data#checksum

maxPacketSize = 1000
defaultBuffer = 4096
defaultTimeout = 1000000

recvTimed :: Socket -> IO (Maybe B.ByteString)
recvTimed = timeout defaultTimeout . flip SocketB.recv defaultBuffer

recvPacketData :: Socket -> B.ByteString -> IO (Bool, B.ByteString, Maybe B.ByteString)
recvPacketData sock buffer = do
    packetData <- recvTimed sock
    let peerStopped = maybe False BC.null
    if peerStopped packetData then
            return (True, BC.empty, Nothing)
        else
            let (a1, a2) = parsePacket $ maybe buffer (B.append buffer) packetData
                in
            return (False, a1, a2)

parsePacket :: B.ByteString -> (B.ByteString, Maybe B.ByteString)
parsePacket buffer =
    if messageIsGood then
        doParse messageBody messageEnd
    else
        (messageStart, Nothing)
    where
        startSep = toEnum . fromEnum $ '$'
        endSep = toEnum . fromEnum $ '#'
        messageStart = B.dropWhile (/=startSep) buffer
        (messageBody, messageEnd) = B.span (/=endSep) messageStart
        messageIsGood = (not (B.null messageBody)) && (B.length messageEnd >= 2)
        doParse messageBody messageEnd = (messageEnd, Just $ B.tail messageBody)

sendPacketString :: MonadIO m => Socket -> GDBState -> String -> m ()
sendPacketString sock state packet = sendPacket sock state $ BC.pack packet

sendQueueAndWait :: MonadIO m => GDBState -> PrismCpuCommand -> m PrismCpuResponse
sendQueueAndWait state cmd = sendAndWaitCpuMsg (gdbCmdQueue state) (gdbRspQueue state) cmd

sendQueueAndWaitF :: MonadIO m => GDBState -> PrismCpuCommand -> (PrismCpuResponse -> Bool) -> m PrismCpuResponse
sendQueueAndWaitF state cmd ignoreRsp = do
    sendCpuMsgIO (gdbCmdQueue state) cmd
    waitRsp
    where
        waitRsp = do
            rsp <- recvCpuMsgIO (gdbRspQueue state)
            if ignoreRsp rsp then
                waitRsp
                else
                    return rsp

ignoreRspCont PRspCont = True
ignoreRspCont _ = False

-------------------------------------------------------------------------------

regsToString :: RegState -> String
regsToString regs =
    foldl uint16ToHex emptyRegs $ ((map conv [rsES, rsDS, rsSS, rsCS])
    ++ [allFlags]
    ++ [eip]
    ++ (map conv [rsDI, rsSI, rsBP, rsSP, rsBX, rsDX, rsCX, rsAX]))
    where
        conv f = (fromIntegral :: Word16 -> Word32) $ f regs
        allFlags = fromIntegral $
                   flagsToVal (rsFlags regs) $
                   eflagsToVal (rsEFlags regs) 0
        eip = (shiftL (fromIntegral $ rsCS regs) 4) + fromIntegral (rsIP regs)
        uint16ToHex s v = toHex1 v s
        emptyRegs = "0000000000000000"

memToString :: B.ByteString -> String
memToString mem =
    foldl (flip toHex1) "" $ reverse $ B.unpack mem

-------------------------------------------------------------------------------

emptyO = GDBState True 1000

replyEmpty :: Socket -> GDBState -> GDBServer
replyEmpty sock state = sendPacket sock state BC.empty

replyOk :: Socket -> GDBState -> GDBServer
replyOk sock state = sendPacketString sock state "OK"

processCommand :: HasCallStack => GDBPacket -> String -> Socket -> GDBServer
processCommand command commandText sock = do
    state <- SC.get
    case command of
        GQuery GQSupported ->
            sendPacketString sock state $
                "PacketSize=" ++ (show $ gdbPacketSize state)
        GQuery GQTStatus ->
            sendPacketString sock state "T1"
        GQuery GQAttached ->
            sendPacketString sock state "1"
        GQuery GQFThreadInfo ->
            sendPacketString sock state "m1"
        GQuery GQSThreadInfo ->
            sendPacketString sock state "l"
        GQuery GQCurrentThread ->
            sendPacketString sock state "QC01"
        GMustReplyEmpty ->
            replyEmpty sock state
        GReadGRegs -> do
            res <- sendQueueAndWaitF state PCmdReadRegs ignoreRspCont
            case res of
                PRspRegs regs -> do
                    let str = regsToString regs
                    logDebug $ T.pack str
                    sendPacketString sock state str
                r -> do
                    logErrorSH r
                    sendPacketString sock state "E00"
        GReadMem (addr, len) -> do
            res <- sendQueueAndWaitF state (PCmdReadMem addr len) ignoreRspCont
            case res of
                PRspMem mem -> do
                    let str = memToString mem
                    logDebug $ T.pack str
                    sendPacketString sock state str
                r -> do
                    logErrorSH r
                    sendPacketString sock state "E00"
        GReadReg reg ->
            sendPacketString sock state $ take 8 $ cycle (show reg)
        GWriteReg (reg, val) -> do
            logInfoN $ T.pack $ "Write reg " ++ (show reg) ++ " " ++ (show val)
            if reg < 8 then
                sendCpuMsgIO (gdbCmdQueue state) $ PCmdWriteReg16 (Reg16 . fromIntegral $ reg) (fromIntegral val)
                else if reg >= 10 then
                    let regSeg = case reg of
                            10 -> cs
                            11 -> ss
                            12 -> ds
                            13 -> es
                            _ -> cs
                            in
                    sendCpuMsgIO (gdbCmdQueue state) $ PCmdWriteRegSeg regSeg (fromIntegral val)
                    else
                        return ()
            replyOk sock state
        GWriteMem (0, _) -> do
            logInfoN . T.pack $ "Bad write - " ++ commandText
            replyEmpty sock state
        GWriteMem (addr, val) -> do
            sendCpuMsgIO (gdbCmdQueue state) $ PCmdWriteMem addr val
            replyOk sock state
        GStep -> do
            res <- sendQueueAndWaitF state PCmdStep ignoreRspCont
            case res of
                PRspStep -> 
                    sendPacketString sock state "S05"
                r -> do
                    logErrorSH r
                    sendPacketString sock state "E00"
        GCont _ | (gdbNumBreakpoints state) <= 0 -> do
            logInfoN . T.pack $ "Bad cont - " ++ commandText
            res <- sendQueueAndWaitF state PCmdStep ignoreRspCont
            case res of
                PRspStep ->
                    sendPacketString sock state "S05"
                r -> do
                    logErrorSH r
                    sendPacketString sock state "E00"
        GCont _ -> do
            res <- sendQueueAndWait state PCmdCont
            case res of
                PRspCont ->
                    sendPacketString sock state "S05"
                r -> do
                    logErrorSH r
                    sendPacketString sock state "E00"
        GBreakPoint (0, _) -> do
            logInfoN . T.pack $ "Bad breakpoint - " ++ commandText
            replyEmpty sock state
        GBreakPoint (addr, type_) -> do
            logInfoN $ T.pack $ "Breakpoint " ++ (show addr) ++ " " ++ (show type_)
            replyOk sock state
            sendCpuMsgIO (gdbCmdQueue state) $ PCmdBreak addr
            SC.put $ state { gdbNumBreakpoints = (gdbNumBreakpoints state) + 1 }
            return ()
        GBreakPointRemove (addr, type_) -> do
            logInfoN $ T.pack $ "Breakpoint remove " ++ (show addr) ++ " " ++ (show type_)
            replyOk sock state
            sendCpuMsgIO (gdbCmdQueue state) $ PCmdBreakRemove addr
            SC.put $ state { gdbNumBreakpoints = (gdbNumBreakpoints state) - 1 }
            return ()
        GHaltReason ->
            sendPacketString sock state "S05"
        GThreadOp ->
            replyOk sock state
        GThreadSetOp ->
            replyOk sock state
        GDetach ->
            replyOk sock state
        GKill ->
            replyOk sock state
        _ -> do
            logInfoN . T.pack $ "Unknown command - " ++ commandText
            replyEmpty sock state

processMessage :: HasCallStack => Socket -> String -> GDBServer
processMessage sock message = do
    logDebugSH message
    let commandC = parse gdbParser "" (T.pack message)
    case commandC of
        Right c ->
            processCommand c message sock
        Left error -> do
            (logInfo $ T.pack ("GDB parsing error " ++ message)) >> logDebugSH error
            state <- SC.get
            replyEmpty sock state

-------------------------------------------------------------------------------

sendPacket :: MonadIO m => Socket -> GDBState -> B.ByteString -> m ()
sendPacket sock state packet = liftIO $ SocketB.send sock packetData >> return ()
    where
        ackEnabled = gdbAckEnabled state
        checksum = showHex (B.foldl (+) 0 packet) ""
        checksumBC = BC.pack $ if length checksum < 2 then '0' : checksum else checksum
        packetData =
            if ackEnabled then
                BC.append (BC.snoc (BC.cons '+' (BC.cons '$' packet)) '#') checksumBC
            else
                BC.append (BC.snoc (BC.cons '$' packet) '#') checksumBC

-------------------------------------------------------------------------------

readMessages :: Socket -> B.ByteString -> GDBServer
readMessages sock buffer = do
    (stop, nb, message) <- liftIO $ recvPacketData sock B.empty
    if stop then do
            logWarn $ T.pack "Peer stopped"
            return ()
        else do
            case message of
                Just m ->
                    processMessage sock $ BC.unpack m
                Nothing -> return ()
            readMessages sock nb

spawnServer :: (Socket, SockAddr) -> GDBServer
spawnServer (sock, addr) = do
    logInfo $ T.pack $ show addr
    readMessages sock B.empty

iterServer :: Socket -> GDBServer
iterServer sock =
    bracket (liftIO $ accept sock) (liftIO . close . fst) (spawnServer) >> iterServer sock

runServer :: String -> Int -> GDBServer
runServer ipAddress port =
    bracket (liftIO $ socket AF_INET Stream defaultProtocol) (liftIO . close) (startServer)
    where
        numClients = 1
        ipAddressOctets = tupleToHostAddress $ Net.IPv4.toOctets $ fromJust $ Net.IPv4.decodeString ipAddress
        sockAddr = SockAddrInet (fromIntegral port) ipAddressOctets
        startServer sock = do
            liftIO (bind sock sockAddr >> listen sock numClients)
            iterServer sock

-------------------------------------------------------------------------------
