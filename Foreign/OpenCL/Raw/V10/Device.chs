{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Device
       ( getDeviceIDs
       , getDeviceInfo
       )
       where

#include <CL/opencl.h>

import Foreign.OpenCL.Raw.Internal.C2HS
import Foreign.OpenCL.Raw.Internal.Types
import Foreign.OpenCL.Raw.Error

#c
enum CLDeviceType {
    DeviceTypeCPU = CL_DEVICE_TYPE_CPU,
    DeviceTypeGPU = CL_DEVICE_TYPE_GPU,
    DeviceTypeAccelerator = CL_DEVICE_TYPE_ACCELERATOR,
    DeviceTypeDefault = CL_DEVICE_TYPE_DEFAULT,
};
#endc
{#enum CLDeviceType as DeviceType {} deriving (Eq,Show,Read)#}

deviceTypeAll :: [DeviceType]
deviceTypeAll = [DeviceTypeCPU, DeviceTypeGPU, DeviceTypeAccelerator, DeviceTypeDefault]

{#fun unsafe clGetDeviceIDs as getDeviceIDs
  { id `Ptr ()' -- to be ignored
  , combineBitMasks `[DeviceType]'
  , `Int'
  , castPtr `Ptr (Ptr _CLDeviceID)'
  , alloca- `Int' peekIntConv*
  } -> `Int' checkSuccess*-
#}

{-|
(
cl_platform_id platform,
cl_device_type device_type,
cl_uint num_entries,
cl_device_id *devices,
cl_uint *num_devices
)
-}

{#fun unsafe clGetDeviceInfo as getDeviceInfo
 { deviceIDPtr `DeviceID'
 , `Int'
 , `Int'
 , castPtr `Ptr a'
 , alloca- `Int' peekIntConv*
 } -> `Int' checkSuccess*-
#}
