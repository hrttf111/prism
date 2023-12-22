module Assembler
    (
      makeAsm,
      makeAsmStr
    ) where

import System.IO (hFlush, hClose, withFile, readFile', IOMode( ReadMode ) )
import System.Exit (ExitCode( ExitSuccess ))

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import System.Posix.Files (ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.IO (stdOutput, stdError, dupTo, openFd, closeFd, OpenMode(WriteOnly), defaultFileFlags)
import System.Posix.Process (ProcessStatus( Exited ), executeFile, forkProcess, getProcessStatus)
import System.Posix.Temp (mkstemp)
import qualified Control.Exception as E
import System.Directory (removeFile)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

-------------------------------------------------------------------------------

makeAsmStr :: Text -> IO B.ByteString
makeAsmStr text = makeAsm $ encodeUtf8 text

-------------------------------------------------------------------------------

makeAsm :: BC.ByteString -> IO B.ByteString
makeAsm input = 
    E.bracket mkLibFile removePath
        (\pathLib -> E.bracket mkLogFile removePath
            ((\pathLib pathLog -> E.bracket mkTempFile closeAndRemove $ m 0 pathLib pathLog) pathLib))
    where
        removePath path = E.catch (removeFile path) (\(ex :: E.IOException) -> return())
        closeAndRemove (f, h) = hClose h >> removeFile f
        mkLibFile = return "/tmp/asmhs_lib"
        mkTempFile = mkstemp "/tmp/asmhs_temp_"
        mkLogFile = return "/tmp/asmhs_log"
        m 10 _ _ _ = return B.empty
        m n pathLib pathLog pathTemp = do
            result <- execAsm pathLib pathLog pathTemp
            case result of
                Just code -> return code
                Nothing -> m (n+1) pathLib pathLog pathTemp
        execAsm pathLib pathLog (path, h) = do
            B.hPutStr h input >> hFlush h
            logFd <- openFd pathLog WriteOnly (Just $ unionFileModes ownerWriteMode ownerReadMode) defaultFileFlags
            (forkProcess $ openAsm path pathLib logFd)
                >>= getProcessStatus True True
                >>= processStatus pathLib pathLog logFd
        processStatus path _ logFd (Just (Exited ExitSuccess)) = do
            closeFd logFd
            Just <$> withFile path ReadMode B.hGetContents
        processStatus _ pathLog logFd _ = do
            closeFd logFd
            content <- readFile' pathLog
            putStrLn content
            return Nothing
        openAsm path pathLib logFd = do
            dupTo logFd stdOutput
            dupTo logFd stdError
            executeFile "yasm" True (opts path pathLib) Nothing
        opts path pathLib = ["-f", "bin", "-p", "nasm", path, "-o", pathLib]

-------------------------------------------------------------------------------
