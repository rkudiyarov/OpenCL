{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Device
       ( clGetDeviceIDs
       , clGetDeviceInfo
       , CLDeviceType(..)
       , CLDeviceInfo(..)
       )
       where

#include "../inc_opencl.h"

import Foreign.OpenCL.Raw.C2HS
import Foreign.OpenCL.Raw.V10.Types
import Foreign.OpenCL.Raw.V10.Error

#c
enum CLDeviceType {
    CLDeviceTypeCPU = CL_DEVICE_TYPE_CPU,
    CLDeviceTypeGPU = CL_DEVICE_TYPE_GPU,
    CLDeviceTypeAccelerator = CL_DEVICE_TYPE_ACCELERATOR,
    CLDeviceTypeDefault = CL_DEVICE_TYPE_DEFAULT,
    CLDeviceTypeAll = CL_DEVICE_TYPE_ALL,
};
#endc

{#enum CLDeviceType {} deriving (Eq, Show) #}

clDeviceType = cIntConv . fromEnum

{#fun unsafe clGetDeviceIDs as ^
  { castPtr `CLPlatformID'
  , clDeviceType `CLDeviceType'
  , `Int'
  , castPtr `Ptr CLDeviceID'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

#c
enum CLDeviceInfo {
    CLDeviceType = CL_DEVICE_TYPE,
};
#endc

{#enum CLDeviceInfo {} deriving (Eq, Show) #}

clDeviceInfo = cFromEnum

{#fun unsafe clGetDeviceInfo as ^
 { castPtr `CLDeviceID'
 , clDeviceInfo `CLDeviceInfo'
 , `Int'
 , castPtr `Ptr a'
 , alloca- `Int' peekIntConv*
 } -> `Int' checkSuccess*-
#}
