--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2010..2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Device where

#include "../inc_opencl.h"

import Foreign.OpenCL.Raw.C2HS
import Foreign.OpenCL.Raw.V10.Types

{#fun unsafe clGetDeviceIDs as ^
  { cl_platform_id `CLPlatformId'
  , cl_device_type `CLDeviceType'
  , cl_uint        `CLuint'
  , castPtr        `Ptr CLDeviceId'
  , castPtr        `Ptr CLuint'
  } -> `CLint' cl_int
#}

{#fun unsafe clGetDeviceInfo as ^
 { cl_device_id   `CLDeviceId'
 , cl_device_info `CLDeviceInfo'
 , size_t         `CLsize'
 , castPtr        `Ptr a'
 , castPtr        `Ptr CLsize'
 } -> `CLint' cl_int
#}
