-- A program to test creating a message queue.
-- 
-- Based on sipc code from: examples/mq_creator.c
--
module Main where

import Bindings.SELinux.SIPC
import Foreign.Ptr
import System.IO

main :: IO ()
main = do
 -- TODO: need to tweak naming conventions a bit
  sipc <- sipcOpen "sipc_mq_test" SipcCreator SipcSysvMqueues 0
  if sipc == nullPtr
     then do
        hPutStrLn stderr "Error: Unable to create message queue"
     else do
        sipc_close(sipc)
