--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2010..2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Platform
       ( CLDPlatformInfo(..)
       , clPlatformInfo
       , clGetPlatformIDs
       , clGetPlatformInfo
       )
       where

#include "../inc_opencl.h"

import Foreign.OpenCL.Raw.C2HS
import Foreign.OpenCL.Raw.V10.Types

#c
enum CLDPlatformInfo {
    CLPlatformProfile    = CL_PLATFORM_PROFILE,
    CLPlatformVersion    = CL_PLATFORM_VERSION,
    CLPlatformName       = CL_PLATFORM_NAME,
    CLPlatformVendor     = CL_PLATFORM_VENDOR,
    CLPlatformExtensions = CL_PLATFORM_EXTENSIONS,
};
#endc

{#enum CLDPlatformInfo {} deriving (Eq, Show) #}

clPlatformInfo a = fromEnum a

{#fun unsafe clGetPlatformIDs as ^
  { cl_uint `CLuint'
  , castPtr `Ptr CLPlatformID'
  , castPtr `Ptr CLuint'
  } -> `CLint' cl_int
#}

{#fun unsafe clGetPlatformInfo as ^
  { cl_platform_id   `CLPlatformID'
  , cl_platform_info `CLPlatformInfo'
  , size_t           `CLsize'
  , castPtr          `Ptr a'
  , castPtr          `Ptr CLsize'
  } -> `CLint' cl_int
#}
