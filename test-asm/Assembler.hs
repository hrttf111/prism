module Assembler
    (
      makeAsm,
      makeAsmStr
    ) where

import System.IO (hFlush, hClose, withFile, IOMode( ReadMode ) )
import System.Exit (ExitCode( ExitSuccess ))

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import System.Posix.IO (stdOutput, stdError, dupTo, openFd, OpenMode(WriteOnly), defaultFileFlags)
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
    E.bracket mkLibFile removeLib
        (\f1 -> E.bracket mkTempFile closeAndRemove $ m 0 f1)
    where
        removeLib path = E.catch (removeFile path) (\(ex :: E.IOException) -> return())
        closeAndRemove (f, h) = hClose h >> removeFile f
        mkLibFile = return "/tmp/asmhs_lib"
        mkTempFile = mkstemp "/tmp/asmhs_temp_"
        m 10 _ _ = return B.empty
        m n pathLib pathTemp = do
            result <- execAsm pathLib pathTemp
            case result of
                Just code -> return code
                Nothing -> m (n+1) pathLib pathTemp
        execAsm pathLib (path, h) = do
            B.hPutStr h input >> hFlush h
            (forkProcess $ openAsm path pathLib)
                >>= getProcessStatus True True
                >>= processStatus pathLib
        processStatus path (Just (Exited ExitSuccess)) =
            Just <$> withFile path ReadMode B.hGetContents
        processStatus _ _ = return Nothing
        openAsm path pathLib = do
            nullFd <- openFd "/dev/null" WriteOnly Nothing defaultFileFlags
            dupTo nullFd stdOutput
            dupTo nullFd stdError
            executeFile "yasm" True (opts path pathLib) Nothing
        opts path pathLib = ["-f", "bin", "-p", "nasm", path, "-o", pathLib]

-------------------------------------------------------------------------------
