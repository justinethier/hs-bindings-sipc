-- A program to test destroying a message queue.
-- 
-- Based on sipc code from: examples/mq_destroyer.c
--
module Main where

import Bindings.SELinux.SIPC
import Foreign.Ptr
import System.IO

main :: IO ()
main = do
  sipcUnlink "sipc_mq_test" SipcSysvMqueues 
