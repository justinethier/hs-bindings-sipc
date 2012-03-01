{- |
Module      : Bindings.SELinux.SIPC
Copyright   : Justin Ethier
License     : LGPL

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module provides Haskell bindings for the SELinux SIPC library.

-}

{-# LANGUAGE ForeignFunctionInterface#-}

module Bindings.SELinux.SIPC
    ( SipcRole(..)
    , SipcType(..)
    , SipcIOCtl(..)
    , SipcPtr
    , sipcOpen
    , sipcClose
    , sipcUnlink
    , sipcIoctl
    , sipcSendData 
    , sipcGetDataPtr
    , sipcRecvData
    , sipcShmRecvDone
    ) where

import Foreign
import Foreign.C

-- The sipc header must be installed on your system
-- in order to compile this module!!
#include <sipc/sipc.h>

#c
enum SipcRole {
 SipcCreator  = SIPC_CREATOR,
 SipcSender   = SIPC_SENDER,
 SipcReceiver = SIPC_RECEIVER
};
enum SipcType {
 SipcSysvShm     = SIPC_SYSV_SHM,
 SipcSysvMqueues = SIPC_SYSV_MQUEUES,
 SipcNumTypes    = SIPC_NUM_TYPES
};
enum SipcIOCtl {
 SipcBlock   = SIPC_BLOCK,
 SipcNoblock = SIPC_NOBLOCK
};
#endc

-- |SIPC Roles
{#enum SipcRole {} deriving (Eq, Show)#}
-- |SIPC Types
{#enum SipcType {} deriving (Eq, Show)#}
-- |SIPC behaviors, for sipc_ioctl()
{#enum SipcIOCtl {} deriving (Eq, Show)#}

-- |A pointer to the sipc struct type
type SipcPtr = Ptr ()

{-
/* sipc_open(char *key, int role, int ipc_type, size_t size)
 *	key		Unique key to identify IPC communication channel,
 *			must be the same for the sender and reciever.
 *	role		SIPC_CREATOR if this application is the IPC creator,
 *			SIPC_SENDER for the sender and SIPC_RECEIVER
 *			for the reciever
 *	ipc_type	Which IPC type used, must be the same for sender
 *			and reciever.
 *	size		Maximum message size to be transmitted.
 *
-}

-- |This function must be called before any other function to initialize
--  the sipc struct.  Caller must call sipcClose to free the memory.
{#fun unsafe sipc_open as ^ {
    `String',               -- ^ Unique key to identify the communication channel 
    cFromEnum `SipcRole', 
    cFromEnum `SipcType', 
    fromIntegral `CSize' 
  } -> `SipcPtr' id #}

-- |Free the sipc struct and its contents
{#fun unsafe sipc_close as ^ {id `SipcPtr'} -> `()' #}

-- |Called by a helper application to destroy an IPC resource.
{#fun unsafe sipc_unlink as ^ {`String', cFromEnum `SipcType'} -> `()' #}

-- |Modify the behavior of an open SIPC channel.  Returns 0 on success,
--  <0 on failure.
{#fun unsafe sipc_ioctl as ^ {id `SipcPtr', cFromEnum `SipcIOCtl'} -> `Int' #}

-- |Blocking call to send msglen bytes of data.
--  This can only be called if sender was specified when sipcOpen was called.
--  returns 0 on success, <0 on failure
{#fun unsafe sipc_send_data as ^ {id `SipcPtr', fromIntegral `CSize'} -> `Int' #}

-- |Returns a pointer to the data contained within the IPC resource
{#fun unsafe sipc_get_data_ptr as ^ {id `SipcPtr'} -> `Ptr CChar' id #}

-- |Call to receive data.  Data will be allocated and filled and len
--  will be set to the length.  Returns 0 on success, <0 on failure.
sipcRecvData :: SipcPtr -> IO (Int, Ptr CChar, CSize)
sipcRecvData a1 =
  let {a1' = id a1} in
  alloca $ \a2' ->
  alloca $ \a3' ->
  sipcRecvData'_ a1' a2' a3' >>= \res ->
  peek  a2'>>= \a2'' ->
  peek  a3'>>= \a3'' ->
  let {res' = fromIntegral res} in
  return (res', a2'', fromIntegral a3'')
foreign import ccall unsafe "Bindings/SELinux/SIPC.chs.h sipc_recv_data"
  sipcRecvData'_ :: ((Ptr ()) -> ((Ptr (Ptr CChar)) -> ((Ptr CUInt) -> (IO CInt))))

-- FUTURE: It would be nice to use the following directive to generate 
--         sipcRecvData, but CSize maps to either CUInt (32-bit) or
--         CULong (64-bit) and I can only get this #fun to work by 
--         hardcoding one of those types instead of CSize: 
--{#fun unsafe sipc_recv_data as ^ {id `SipcPtr', alloca- `Ptr CChar' peek*, alloca- `CSize' peek*} -> `Int' #}

-- |Receiver calls this when it is done receiving a message in shared
--  memory.  There must be exactly one call to sipcShmRecvDone() for
--  every call to sipcRecvData(), if the sipc was created as shared
--  memory.
{#fun unsafe sipc_shm_recv_done as ^ {id `SipcPtr'} -> `Int' #}

-- |Convert a Haskell enumeration to C.
-- 
--  This code is from C2HS, but it has been added
--  here since C2HS is deprecated.
cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = fromIntegral . fromEnum

