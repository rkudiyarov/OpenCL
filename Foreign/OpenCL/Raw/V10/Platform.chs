{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Platform
       ( clGetPlatformIDs
       , clGetPlatformInfo
       , CLPlatformInfo(..)
       )
       where

#include "../inc_opencl.h"

import Foreign.OpenCL.Raw.C2HS
import Foreign.OpenCL.Raw.V10.Types
import Foreign.OpenCL.Raw.V10.Error

{#fun unsafe clGetPlatformIDs as ^
  { `Int'
  , castPtr `Ptr CLPlatformID'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

#c
enum CLPlatformInfo {
    CLPlatformProfile = CL_PLATFORM_PROFILE,
    CLPlatformVersion = CL_PLATFORM_VERSION,
    CLPlatformName = CL_PLATFORM_NAME,
    CLPlatformVendor = CL_PLATFORM_VENDOR,
    CLPlatformExtensions = CL_PLATFORM_EXTENSIONS,
};
#endc

{#enum CLPlatformInfo {} deriving (Eq, Show) #}

fromEnumToInt = cIntConv . fromEnum

{#fun unsafe clGetPlatformInfo as ^
  { castPtr `CLPlatformID'
  , fromEnumToInt `CLPlatformInfo'
  , `Int'
  , castPtr `Ptr a'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}
