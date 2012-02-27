-- A program to test creating a message queue.
-- 
-- Based on sipc code from: examples/mq_creator.c
--
module Main where

import Bindings.SELinux.SIPC

main :: IO ()
main = do
  sipc <- sipc_open "sipc_mq_test" Sipc_creator Sipc_sysv_mqueues 0
  -- TODO: how to test for NULL pointer?
  --puStrLn $ show sipc
  sipc_close(sipc)
  putStrLn ""
