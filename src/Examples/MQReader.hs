-- A program to test receiving messages on a message queue.
-- 
-- Based on sipc code from: examples/mq_reader.c
--
module Main where

import Bindings.SELinux.SIPC
import Control.Monad (when)
import Foreign
import Foreign.C
import System.IO

-- Key which sender and receiver have agreed upon 
sipcKey = "sipc_mq_test"

-- Amount of data to allocate inside the IPC handle
dataLen = 8192

-- End of message marker which sender and receiver have agreed upon
dataEnd = "0xDEADBEEF"  

main :: IO ()
main = do
  sipc <- sipcOpen sipcKey SipcReceiver SipcSysvMqueues dataLen
  if sipc == nullPtr
     then do
        hPutStrLn stderr "Error: Unable to create message queue"
     else do
        recvMessages sipc
        sipcClose sipc

-- |Receive data from shared memory until the end of transmission
--  marker has been received.
recvMessages :: SipcPtr -> IO ()
recvMessages sipc = do
    (result, dataP, len) <- sipcRecvData sipc
    when (result == 0) $ recv dataP
 where
    recv dataP = do
        dataStr <- peekCString dataP
        free dataP
        when (dataStr /= dataEnd) $ recvNext dataStr
    recvNext msg = do
        putStr msg
        recvMessages sipc
