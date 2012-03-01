-- A program to test sending messages to a message queue.
-- 
-- Based on sipc code from: examples/mq_sender.c
--
module Main where

import Bindings.SELinux.SIPC
import Control.Monad (when)
import Foreign
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import System.IO

-- Key which sender and receiver have agreed upon 
sipcKey = "sipc_mq_test"

-- # of bytes we allocate for the IPC's internal buffer.
-- This should be >= the amount of data copied into the IPC 
ipcLen = 8192

-- # of bytes to read from data file at a time
readLen = 4096

-- Amount of data to allocate inside the IPC handle
dataLen = 8192

-- End of message marker which sender and receiver have agreed upon
dataEnd = "0xDEADBEEF"  

-- Data file
inFile = "data.txt"

main :: IO ()
main = do
  sipc <- sipcOpen "sipc_mq_test" SipcSender SipcSysvMqueues ipcLen 
  if sipc == nullPtr
     then do
        hPutStrLn stderr "Error: Unable to create message queue"
        sipcClose sipc
     else do
        dataP <- sipcGetDataPtr sipc
        if dataP == nullPtr
           then do
              hPutStrLn stderr "Error: Unable to get data pointer"
              sipcClose sipc
           else do 
              sendFileData sipc dataP
              sendEndXmit sipc dataP
              sipcClose sipc

-- |Read and enqueue file data
sendFileData :: SipcPtr -> Ptr CChar -> IO ()
sendFileData sipc dataP = do
  f <- openFile inFile ReadMode
  fData <- hGetContents f
  sendFileChunks sipc dataP fData
  hClose f

-- |Send all chunks of a file to the queue
sendFileChunks sipc dataP chunkList = do
  let (c, cs) = splitAt readLen chunkList
  clearBuffer dataP
  sendMessage sipc dataP c
  when (not $ null cs) $ sendFileChunks sipc dataP cs

-- |Send data to the message queue
sendMessage :: SipcPtr -> Ptr CChar -> String -> IO ()
sendMessage sipc dataP msg = do
    tmpP <- newArray $ map castCharToCChar msg
    _ <- copyBytes dataP tmpP $ length msg
    free tmpP
    result <- sipcSendData sipc ipcLen
    when (result < 0) $ putStrLn $ "Unable to send IPC message"

-- |Send end of transmission marker
sendEndXmit sipc dataP = do
    tmpP <- newCAString dataEnd
    _ <- copyBytes dataP tmpP $ 1 + length dataEnd
    free tmpP
    result <- sipcSendData sipc ipcLen
    when (result < 0) $ putStrLn $ "Unable to send IPC message"

-- FUTURE: just replace this with memcpy?
-- A quick-and-dirty bzero
clearBuffer dataP = do
    let buffer = take (fromIntegral ipcLen) $ repeat '\0'
    tmpP <- newArray $ map castCharToCChar buffer
    _ <- copyBytes dataP tmpP $ length buffer
    free tmpP

