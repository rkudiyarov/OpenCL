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
    CLDeviceType                       = CL_DEVICE_TYPE,
    CLDeviceVendorId                   = CL_DEVICE_VENDOR_ID,
    CLDeviceMaxComputeUnits            = CL_DEVICE_MAX_COMPUTE_UNITS,
    CLDeviceMaxWorkItemDimensions      = CL_DEVICE_MAX_WORK_ITEM_DIMENSIONS,
    CLDeviceMaxWorkItemSizes           = CL_DEVICE_MAX_WORK_ITEM_SIZES,
    CLDeviceMaxWorkGroupSize           = CL_DEVICE_MAX_WORK_GROUP_SIZE,
    CLDevicePreferredVectorWidthChar   = CL_DEVICE_PREFERRED_VECTOR_WIDTH_CHAR,
    CLDevicePreferredVectorWidthShort  = CL_DEVICE_PREFERRED_VECTOR_WIDTH_SHORT,
    CLDevicePreferredVectorWidthInt    = CL_DEVICE_PREFERRED_VECTOR_WIDTH_INT,
    CLDevicePreferredVectorWidthLong   = CL_DEVICE_PREFERRED_VECTOR_WIDTH_LONG,
    CLDevicePreferredVectorWidthFloat  = CL_DEVICE_PREFERRED_VECTOR_WIDTH_FLOAT,
    CLDevicePreferredVectorWidthDouble = CL_DEVICE_PREFERRED_VECTOR_WIDTH_DOUBLE,
    CLDeviceMaxClockFrequency          = CL_DEVICE_MAX_CLOCK_FREQUENCY,
    CLDeviceAddressBits                = CL_DEVICE_ADDRESS_BITS,
    CLDeviceMaxMemAllocSize            = CL_DEVICE_MAX_MEM_ALLOC_SIZE,
    CLDeviceImageSupport               = CL_DEVICE_IMAGE_SUPPORT,
    CLDeviceMaxReadImageArgs           = CL_DEVICE_MAX_READ_IMAGE_ARGS,
    CLDeviceMaxWriteImageArgs          = CL_DEVICE_MAX_WRITE_IMAGE_ARGS,
    CLDeviceImage2dMaxWidth            = CL_DEVICE_IMAGE2D_MAX_WIDTH,
    CLDeviceImage2dMaxHeight           = CL_DEVICE_IMAGE2D_MAX_HEIGHT,
    CLDeviceImage3dMaxWidth            = CL_DEVICE_IMAGE3D_MAX_WIDTH,
    CLDeviceImage3dMaxHeight           = CL_DEVICE_IMAGE3D_MAX_HEIGHT,
    CLDeviceImage3dMaxDepth            = CL_DEVICE_IMAGE3D_MAX_DEPTH,
    CLDeviceMaxSamplers                = CL_DEVICE_MAX_SAMPLERS,
    CLDeviceMaxParameterSize           = CL_DEVICE_MAX_PARAMETER_SIZE,
    CLDeviceMemBaseAddrAlign           = CL_DEVICE_MEM_BASE_ADDR_ALIGN,
    CLDeviceMinDataTypeAlignSize       = CL_DEVICE_MIN_DATA_TYPE_ALIGN_SIZE,
    CLDeviceSingleFpConfig             = CL_DEVICE_SINGLE_FP_CONFIG,
    CLDeviceGlobalMemCacheType         = CL_DEVICE_GLOBAL_MEM_CACHE_TYPE,
    CLDeviceGlobalMemCachelineSize     = CL_DEVICE_GLOBAL_MEM_CACHELINE_SIZE,
    CLDeviceGlobalMemCacheSize         = CL_DEVICE_GLOBAL_MEM_CACHE_SIZE,
    CLDeviceGlobalMemSize              = CL_DEVICE_GLOBAL_MEM_SIZE,
    CLDeviceMaxConstantBufferSize      = CL_DEVICE_MAX_CONSTANT_BUFFER_SIZE,
    CLDeviceMaxConstantArgs            = CL_DEVICE_MAX_CONSTANT_ARGS,
    CLDeviceLocalMemType               = CL_DEVICE_LOCAL_MEM_TYPE,
    CLDeviceLocalMemSize               = CL_DEVICE_LOCAL_MEM_SIZE,
    CLDeviceErrorCorrectionSupport     = CL_DEVICE_ERROR_CORRECTION_SUPPORT,
    CLDeviceProfilingTimerResolution   = CL_DEVICE_PROFILING_TIMER_RESOLUTION,
    CLDeviceEndianLittle               = CL_DEVICE_ENDIAN_LITTLE,
    CLDeviceAvailable                  = CL_DEVICE_AVAILABLE,
    CLDeviceCompilerAvailable          = CL_DEVICE_COMPILER_AVAILABLE,
    CLDeviceExecutionCapabilities      = CL_DEVICE_EXECUTION_CAPABILITIES,
    CLDeviceQueueProperties            = CL_DEVICE_QUEUE_PROPERTIES,
    CLDevicePlatform                   = CL_DEVICE_PLATFORM,
    CLDeviceName                       = CL_DEVICE_NAME,
    CLDeviceVendor                     = CL_DEVICE_VENDOR,
    CLDriverVersion                    = CL_DRIVER_VERSION,
    CLDeviceProfile                    = CL_DEVICE_PROFILE,
    CLDeviceVersion                    = CL_DEVICE_VERSION,
    CLDeviceExtensions                 = CL_DEVICE_EXTENSIONS,
};
#endc

{#enum CLDDeviceInfo {} deriving (Eq, Show) #}

clDeviceInfo a = cFromEnum a
