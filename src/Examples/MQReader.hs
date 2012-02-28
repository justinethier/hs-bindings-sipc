-- A program to test creating a message queue.
-- 
-- Based on sipc code from: examples/mq_reader.c
--
module Main where

import Bindings.SELinux.SIPC
import Foreign.Ptr
import System.IO

-- Key which sender and receiver have agreed upon 
SIPC_KEY = "sipc_mq_test"

-- Amount of data to allocate inside the IPC handle
DATA_LEN = 8192

-- End of message marker which sender and receiver have agreed upon
DATA_END = "0xDEADBEEF"  

main :: IO ()
main = do
  sipc <- sipcOpen "sipc_mq_test" SipcCreator SipcSysvMqueues DATA_LEN
  if sipc == nullPtr
     then do
        hPutStrLn stderr "Error: Unable to create message queue"
 	recv
     else do
        sipcClose(sipc)

-- TODO:

recv :: IO()
recv = do

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

