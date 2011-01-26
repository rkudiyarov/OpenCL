--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2010..2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Platform where

#include "../inc_opencl.h"

import Foreign.OpenCL.Raw.C2HS
import Foreign.OpenCL.Raw.V10.Types

{#fun unsafe clGetPlatformIDs as ^
  { cl_uint `CLuint'
  , castPtr `Ptr CLPlatformId'
  , castPtr `Ptr CLuint'
  } -> `CLint' cl_int
#}

{#fun unsafe clGetPlatformInfo as ^
  { cl_platform_id   `CLPlatformId'
  , cl_platform_info `CLPlatformInfo'
  , size_t           `CLsize'
  , castPtr          `Ptr a'
  , castPtr          `Ptr CLsize'
  } -> `CLint' cl_int
#}
