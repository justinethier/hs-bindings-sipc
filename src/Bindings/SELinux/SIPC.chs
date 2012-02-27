{-# LANGUAGE ForeignFunctionInterface#-}

module Bindings.SELinux.SIPC where

import Foreign
import Foreign.C

-- The sipc header must be installed on your system
-- in order to compile this module!!
#include <sipc/sipc.h>

#c
enum SIPCRole {
 Sipc_creator  = SIPC_CREATOR,
 Sipc_sender   = SIPC_SENDER,
 Sipc_receiver = SIPC_RECEIVER
};
enum SIPCType {
 Sipc_sysv_shm     = SIPC_SYSV_SHM,
 Sipc_sysv_mqueues = SIPC_SYSV_MQUEUES,
 Sipc_num_types    = SIPC_NUM_TYPES
};
enum SIPCIOCtl {
 Sipc_block   = SIPC_BLOCK,
 Sipc_noblock = SIPC_NOBLOCK
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
{#fun unsafe sipc_open {`String', cFromEnum `SIPCRole', cFromEnum `SIPCType', `Int' } -> `SipcPtr' id #}
{#fun unsafe sipc_close {id `SipcPtr'} -> `()' #}

{#fun unsafe sipc_unlink {`String', cFromEnum `SIPCType'} -> `()' #}

{#fun unsafe sipc_ioctl {id `SipcPtr', cFromEnum `SIPCIOCtl'} -> `Int' #}

-- TODO: same issue with CSize as above
--int sipc_send_data(sipc_t *sipc, size_t msg_len);
{#fun unsafe sipc_send_data {id `SipcPtr', `Int'} -> `Int' #}

-- TODO: is String really the right return type here??
-- /* Returns a pointer to the data contained within the IPC resource */
--char *sipc_get_data_ptr(sipc_t *sipc);
{#fun unsafe sipc_get_data_ptr {id `SipcPtr'} -> `String' #}

{- TODO:

-- data is allocated by C, believe this must be indicated to Haskell
-- TBD: what about len??
int sipc_recv_data(sipc_t *sipc, char **data, size_t *len);

-- TODO: are variable-length args even supported by the Haskell FFI???
/* Prints an error message, accepts printf format string */
void sipc_error(sipc_t *sipc, const char *fmt, ...)
	__attribute__ ((format(printf, 2, 3)));
-}

{#fun unsafe sipc_shm_recv_done {id `SipcPtr'} -> `Int' #}

-- |Convert a Haskell enumeration to C.
-- 
--  This code is from C2HS, but it has been added
--  here since C2HS is deprecated.
cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = fromIntegral . fromEnum

