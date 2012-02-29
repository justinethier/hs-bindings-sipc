-- A program to test creating a message queue.
-- 
-- Based on sipc code from: examples/mq_reader.c
--
module Main where

import Bindings.SELinux.SIPC
import Foreign
import Foreign.C
--import Foreign.Ptr
import System.IO

-- Key which sender and receiver have agreed upon 
sipcKey = "sipc_mq_test"

-- Amount of data to allocate inside the IPC handle
dataLen = 8192

-- End of message marker which sender and receiver have agreed upon
dataEnd = "0xDEADBEEF"  

main :: IO ()
main = do
  sipc <- sipcOpen "sipc_mq_test" SipcCreator SipcSysvMqueues dataLen
  if sipc == nullPtr
     then do
        hPutStrLn stderr "Error: Unable to create message queue"
     else do
-- 	    _ <- recv sipc
        sipcClose sipc

-- TODO:

recv :: SipcPtr -> IO()
recv sipc = do
    (result, dataP, len) <- sipcRecvData sipc
--    dataStr <- map castCCharToChar dataP
--    putStrLn $ "Received: " ++ dataStr
    free dataP

{-
	/* Receive data from shared memory until the end of transmission
	 * marker has been received. */
	while (!sipc_recv_data(ipc, &data, &msglen)) {
		if (END_XMIT(data))
			break;
		printf("%s", data);

		free(data);
		data = NULL;
	}

	/* Cleanup */
	sipc_close(ipc);
	return 0;
}

static int END_XMIT(char *data)
{
	if (!data)
		return 0;

	return !strcmp(data, DATA_END);
}
-}
