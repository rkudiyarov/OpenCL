--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2010..2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Platform
       ( clGetPlatformIDs
       , clGetPlatformInfo
       )
       where

#include <inc_opencl.h>

import Foreign.OpenCL.Raw.C2HS
import Foreign.OpenCL.Raw.V10.Types

{#fun unsafe clGetPlatformIDs as ^
  { cl_uint `CL_uint'
  , castPtr `Ptr CL_platform_id'
  , castPtr `Ptr CL_uint'
  } -> `CL_int' cl_int
#}

{#fun unsafe clGetPlatformInfo as ^
  { cl_platform_id   `CL_platform_id'
  , cl_platform_info `CL_platform_info'
  , cl_size          `CL_size'
  , castPtr          `Ptr a'
  , castPtr          `Ptr CL_size'
  } -> `CL_int' cl_int
#}
