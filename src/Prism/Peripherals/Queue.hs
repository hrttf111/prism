{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Prism.Peripherals.Queue where

import Control.Concurrent.STM.TQueue

import Prism.Cpu

-------------------------------------------------------------------------------

data IOCmdType = IOMemType | IOPortType deriving (Show)

data IOCmd = IOCmdRead8 IOCmdType IOHandlerIndex MemOffset
             | IOCmdRead16 IOCmdType IOHandlerIndex MemOffset 
             | IOCmdWrite8 IOCmdType IOHandlerIndex MemOffset Uint8 
             | IOCmdWrite16 IOCmdType IOHandlerIndex MemOffset Uint16 
             deriving (Show)

data IOCmdData = IOCmdData8 Uint8
                 | IOCmdData16 Uint16

data IOQueue = IOQueue {
        ioQueueReq :: TQueue IOCmd,
        ioQueueRsp :: TQueue IOCmdData
    }

createIOQueue = IOQueue <$> newTQueueIO <*> newTQueueIO

-------------------------------------------------------------------------------
