module Qemu (
    execQemu
) where

import Control.Concurrent (threadDelay)

import System.IO (withFile, IOMode( ReadMode ) )
import System.Posix.Process (ProcessStatus( Exited ), executeFile, forkProcess, getProcessStatus)
import System.Posix.Signals (signalProcess, sigTERM)
import System.Directory (removeFile)

import qualified Data.ByteString as B

-------------------------------------------------------------------------------

execQemu :: String -> String -> IO (Maybe B.ByteString)
execQemu binPath memFile = do
    childId <- forkProcess execQemu_
    threadDelay sleepTime
    signalProcess childId sigTERM
    childStatus <- getProcessStatus True True childId
    case childStatus of
        Just st -> do
            putStrLn $ "Has status: " ++ (show st)
            memData <- withFile memFile ReadMode B.hGetContents
            removeFile memFile
            return $ Just memData
        Nothing ->
            return Nothing
    where
        sleepTime = 500000 -- 500ms
        --memFile = "mem.data"
        execQemu_ = executeFile "qemu-system-i386" True qemuOpts Nothing
        qemuOpts = ["-fda", binPath,
                     "-m", "1M",
                     "-object", "memory-backend-file,id=pc.ram,size=1M,mem-path="++memFile++",prealloc=on,share=on",
                     "-machine", "memory-backend=pc.ram"
                     ]

-------------------------------------------------------------------------------
