--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2010..2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Device
       ( CLDDeviceType(..)
       , CLDDeviceInfo(..)
       , clGetDeviceIDs
       , clGetDeviceInfo
       )
       where

#include "../inc_opencl.h"

import Foreign.OpenCL.Raw.C2HS
import Foreign.OpenCL.Raw.V10.Types

#c
enum CLDDeviceType {
    CLDeviceTypeCPU         = CL_DEVICE_TYPE_CPU,
    CLDeviceTypeGPU         = CL_DEVICE_TYPE_GPU,
    CLDeviceTypeAccelerator = CL_DEVICE_TYPE_ACCELERATOR,
    CLDeviceTypeDefault     = CL_DEVICE_TYPE_DEFAULT,
    CLDeviceTypeAll         = CL_DEVICE_TYPE_ALL,
};
#endc

{#enum CLDDeviceType {} deriving (Eq, Show) #}

clDeviceType a = fromEnum a

#c
enum CLDDeviceInfo {
    CLDeviceType = CL_DEVICE_TYPE,
};
#endc

{#enum CLDDeviceInfo {} deriving (Eq, Show) #}

clDeviceInfo a = fromEnum a

{#fun unsafe clGetDeviceIDs as ^
  { cl_platform_id `CLPlatformID'
  , cl_device_type `CLDeviceType'
  , cl_uint        `CLuint'
  , castPtr        `Ptr CLDeviceID'
  , castPtr        `Ptr CLuint'
  } -> `CLint' cl_int
#}

{#fun unsafe clGetDeviceInfo as ^
 { cl_device_id   `CLDeviceID'
 , cl_device_info `CLDeviceInfo'
 , size_t         `CLsize'
 , castPtr        `Ptr a'
 , castPtr        `Ptr CLsize'
 } -> `CLint' cl_int
#}
