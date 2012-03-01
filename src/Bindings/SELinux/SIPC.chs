{- |
Module      : Bindings.SELinux.SIPC
Copyright   : Justin Ethier
License     : LGPL

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module provides Haskell bindings for the SIPC library for SELinux.

-}

{-# LANGUAGE ForeignFunctionInterface#-}

module Bindings.SELinux.SIPC where

import Foreign
import Foreign.C

-- The sipc header must be installed on your system
-- in order to compile this module!!
#include <sipc/sipc.h>

#c
enum SIPCRole {
 SipcCreator  = SIPC_CREATOR,
 SipcSender   = SIPC_SENDER,
 SipcReceiver = SIPC_RECEIVER
};
enum SIPCType {
 SipcSysvShm     = SIPC_SYSV_SHM,
 SipcSysvMqueues = SIPC_SYSV_MQUEUES,
 SipcNumTypes    = SIPC_NUM_TYPES
};
enum SIPCIOCtl {
 SipcBlock   = SIPC_BLOCK,
 SipcNoblock = SIPC_NOBLOCK
};
#endc

-- |SIPC Roles
{#enum SIPCRole {} deriving (Eq, Show)#}
-- |SIPC Types
{#enum SIPCType {} deriving (Eq, Show)#}
-- |SIPC behaviors, for sipc_ioctl()
{#enum SIPCIOCtl {} deriving (Eq, Show)#}

-- TODO: documentation for *everything* once all defs are in place

--data Sipc = Sipc 
--type SipcPtr = Ptr Sipc 
type SipcPtr = Ptr ()

-- TODO: should use CSize for last input arg
{#fun unsafe sipc_open as ^ {`String', cFromEnum `SIPCRole', cFromEnum `SIPCType', `Int' } -> `SipcPtr' id #}
{#fun unsafe sipc_close as ^ {id `SipcPtr'} -> `()' #}

{#fun unsafe sipc_unlink as ^ {`String', cFromEnum `SIPCType'} -> `()' #}

{#fun unsafe sipc_ioctl as ^ {id `SipcPtr', cFromEnum `SIPCIOCtl'} -> `Int' #}

-- TODO: same issue with CSize as above
--int sipc_send_data(sipc_t *sipc, size_t msg_len);
{#fun unsafe sipc_send_data as ^ {id `SipcPtr', `Int'} -> `Int' #}

-- /* Returns a pointer to the data contained within the IPC resource */
--char *sipc_get_data_ptr(sipc_t *sipc);
{#fun unsafe sipc_get_data_ptr as ^ {id `SipcPtr'} -> `Ptr CChar' id #}


-- TODO:
-- data is allocated by C, believe this must be indicated to Haskell
-- TBD: what about len??
--
-- /* Call to receive data.  Data will be allocated and filled and len
-- * will be set to the length.  Returns 0 on success, <0 on failure. */
--int sipc_recv_data(sipc_t *sipc, char **data, size_t *len);
{#fun unsafe sipc_recv_data as ^ {id `SipcPtr', alloca- `Ptr CChar' peek*, alloca- `CUInt' peek*} -> `Int' #}

{#fun unsafe sipc_shm_recv_done as ^ {id `SipcPtr'} -> `Int' #}

-- |Convert a Haskell enumeration to C.
-- 
--  This code is from C2HS, but it has been added
--  here since C2HS is deprecated.
cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = fromIntegral . fromEnum

