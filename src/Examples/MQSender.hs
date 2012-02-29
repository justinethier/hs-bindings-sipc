-- A program to test creating a message queue.
-- 
-- Based on sipc code from: examples/mq_sender.c
--
module Main where

import Bindings.SELinux.SIPC
import Control.Monad (when)
import Foreign
import Foreign.C
--import Foreign.Marshall.Utils
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
        sipcClose(sipc)
     else do
        dataP <- sipcGetDataPtr sipc
        if dataP == nullPtr
           then do
              hPutStrLn stderr "Error: Unable to get data pointer"
              sipcClose(sipc)
           else do 
              --sendMessage sipc dataP "TEST STRING. Testing 1, 2, 3..." 
              sendFileData sipc dataP
              sendEndXmit sipc dataP
              sipcClose(sipc)

-- |Send data to the message queue
sendMessage :: SipcPtr -> Ptr CChar -> String -> IO ()
sendMessage sipc dataP msg = do
    tmpP <- newArray $ map castCharToCChar msg
    _ <- copyBytes dataP tmpP $ length msg
    free tmpP
    putStrLn $ "Sending: " ++ msg
    result <- sipcSendData sipc ipcLen
    putStrLn $ "Result: " ++ show result
    -- TODO: error if result < 0

-- |Send end of transmission marker
sendEndXmit sipc dataP = do
    tmpP <- newCAString dataEnd
    _ <- copyBytes dataP tmpP $ 1 + length dataEnd
    free tmpP
    -- TODO: do this as a loop, and check return value
    putStrLn $ "Sending: " ++ dataEnd
    result <- sipcSendData sipc ipcLen
    putStrLn $ "Result: " ++ show result
    -- TODO: error if result < 0


sendFileData :: SipcPtr -> Ptr CChar -> IO ()
sendFileData sipc dataP = do
  f <- openFile inFile ReadMode
  fData <- hGetContents f
  sendFileChunks sipc dataP fData
  hClose f

-- Poor man's bzero
clearBuffer dataP = do
    let buffer = take ipcLen $ repeat '\0'
    tmpP <- newArray $ map castCharToCChar buffer
    _ <- copyBytes dataP tmpP $ length buffer
    free tmpP

sendFileChunks sipc dataP dataList = do
  let (c, cs) = splitAt readLen dataList
  clearBuffer dataP
  sendMessage sipc dataP c
  when (not $ null cs) $ sendFileChunks sipc dataP cs

-- Obsolete:
{-
	int retv = -1;
	FILE *ifile = NULL;
	size_t rbytes = 0;
	char *data = NULL;
	/* Read READ_LEN bytes into the handle's internal buffer */
	while ((rbytes = fread(data, sizeof(char), READ_LEN, ifile)) > 0) {
		  /* Send this chunk of data */
		  if (sipc_send_data(ipc, IPC_LEN) < 0) 
			  sipc_error(ipc, "Unable to send IPC message\n");		  

		  if (feof(ifile))
			  break;

		  /* Clear out internal buffer */
		  bzero(data, IPC_LEN); 
	}
	
	/* Our messages have been sent; send end of transmission marker */
	send_end_xmit(ipc);
	retv = 0;
out:
	/* Cleanup */
	sipc_close(ipc);
	return retv;
}

/* Use this function to send an end of transmission marker */
/* Note that the receiver must be aware of the marker used here */
static int send_end_xmit(sipc_t *ipc)
{
	char *data = sipc_get_data_ptr(ipc);
	if (!data) {
		sipc_error(ipc, 
		    "Unable to get internal data pointer from IPC resource\n");
		return -1;
	}
	
	bzero(data, IPC_LEN);
	strncpy(data, DATA_END, IPC_LEN-1);
	if (sipc_send_data(ipc, IPC_LEN) < 0) {
		sipc_error(ipc, "Unable to send end of transmission marker\n");
		return -1;
	}

	printf("Send end of transmission marker: %s\n:", data);
	return 0;
}

-}
