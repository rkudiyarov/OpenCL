{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Platform
       ( getPlatformIDs
       , getPlatformInfo
       )
       where

#include <CL/opencl.h>

import Foreign.OpenCL.Raw.Internal.C2HS
import Foreign.OpenCL.Raw.Internal.Types
import Foreign.OpenCL.Raw.Error

{#fun clGetPlatformIDs as getPlatformIDs
  { `Int'
  , castPtr `Ptr ()'
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
{#enum CLPlatformInfo {} #}
{#fun clGetPlatformInfo as getPlatformInfo
  { platformIDPtr `PlatformID'
  , cEnum `CLPlatformInfo'
  , `Int'
  , id `Ptr ()'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}
