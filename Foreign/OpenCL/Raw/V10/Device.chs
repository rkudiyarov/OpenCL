--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2010..2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Device
       ( clGetDeviceIDs
       , clGetDeviceInfo
       )
       where

#include <inc_opencl.h>

import Foreign.OpenCL.Raw.C2HS
import Foreign.OpenCL.Raw.V10.Types

{#fun unsafe clGetDeviceIDs as ^
  { cl_platform_id `CL_platform_id'
  , cl_device_type `CL_device_type'
  , cl_uint        `CL_uint'
  , castPtr        `Ptr CL_device_id'
  , castPtr        `Ptr CL_uint'
  } -> `CL_int' cl_int
#}

{#fun unsafe clGetDeviceInfo as ^
 { cl_device_id   `CL_device_id'
 , cl_device_info `CL_device_info'
 , cl_size        `CL_size'
 , castPtr        `Ptr a'
 , castPtr        `Ptr CL_size'
 } -> `CL_int' cl_int
#}
