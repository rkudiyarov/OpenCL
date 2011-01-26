--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2010..2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Types where

#include "../inc_opencl.h"

import Foreign
import Foreign.C
import Foreign.OpenCL.Raw.C2HS

--type CLvoidptr = Prt a
--voidptr = castPtr
type CLsize     = {# type size_t #}
size_t a        = cIntConv a
type CLchar     = {# type cl_char #}
cl_char a       = cIntConv a
type CLuchar    = {# type cl_uchar #}
cl_uchar a      = cIntConv a
type CLshort    = {# type cl_short #}
cl_short a     	= cIntConv a
type CLushort   = {# type cl_ushort #}
cl_ushort a     = cIntConv a
type CLint      = {# type cl_int #}
cl_int a        = cIntConv a
type CLuint     = {# type cl_uint #}
cl_uint a       = cIntConv a
type CLlong     = {# type cl_long #}
cl_long a       = cIntConv a
type CLulong    = {# type cl_ulong #}
cl_ulong a      = cIntConv a
type CLhalf     = {# type cl_half #}
cl_half a       = cIntConv a
type CLfloat    = {# type cl_float #}
cl_float a      = cFloatConv a
type CLdouble   = {# type cl_double #}
cl_double a     = cFloatConv a
type CLbool     = {# type cl_bool #}
cl_bool a       = cl_uint a
type CLbitfield = {# type cl_bitfield #}
cl_bitfield a   = cl_ulong a

type CLPlatformId   = {# type cl_platform_id #}
cl_platform_id a    = castPtr a
type CLPlatformInfo = {# type cl_platform_info #}
cl_platform_info a  = cl_uint a

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

clPlatformInfo a = cFromEnum a

type CLDeviceId   = {# type cl_device_id #}
cl_device_id a    = castPtr a
type CLDeviceType = {# type cl_device_type #}
cl_device_type a  = cl_bitfield a
type CLDeviceInfo = {# type cl_device_info #}
cl_device_info a  = cl_uint a

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

clDeviceType a = cFromEnum a

#c
enum CLDDeviceInfo {
    CLDeviceType            = CL_DEVICE_TYPE,
    CLDeviceVendorId        = CL_DEVICE_VENDOR_ID
};
#endc

{#enum CLDDeviceInfo {} deriving (Eq, Show) #}

clDeviceInfo a = cFromEnum a
