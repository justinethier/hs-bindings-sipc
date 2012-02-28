-- A program to test creating a message queue.
-- 
-- Based on sipc code from: examples/mq_sender.c
--
module Main where

import Bindings.SELinux.SIPC
import Foreign.Ptr
import System.IO

-- Key which sender and receiver have agreed upon 
SIPC_KEY = "sipc_mq_test"

-- # of bytes we allocate for the IPC's internal buffer.
-- This should be >= the amount of data copied into the IPC 
IPC_LEN = 8192

-- # of bytes to read from data file at a time
READ_LEN = 4096

-- Amount of data to allocate inside the IPC handle
DATA_LEN = 8192

-- End of message marker which sender and receiver have agreed upon
DATA_END = "0xDEADBEEF"  

-- Data file
IN_FILE = "data.txt"

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
int main(void)
{
	int retv = -1;
	FILE *ifile = NULL;
	size_t rbytes = 0;
	char *data = NULL;
	sipc_t *ipc = NULL;

	/* Initialize and connect IPC handle */
	ipc = sipc_open(SIPC_KEY, SIPC_SENDER, SIPC_SYSV_MQUEUES, IPC_LEN);
	if (!ipc) {
		fprintf(stderr, "Unable to create IPC resource\n");
		goto out;
	}

	/* Get pointer to the handle's internal buffer */
	data = sipc_get_data_ptr(ipc);
	if (!data) {
		sipc_error(ipc, "Unable to get data pointer\n");
		goto out;
	}

	/* Open data file for reading */
	ifile = fopen(IN_FILE, "r");
	if (!ifile) {
		sipc_error(ipc, "Unable to open data file\n");
		goto out;
	}
	
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


