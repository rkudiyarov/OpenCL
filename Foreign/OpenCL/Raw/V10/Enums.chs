--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Enums where

#include "../inc_opencl.h"

import Foreign.OpenCL.Raw.C2HS

#c
enum CLError {
    CLSuccess                      = CL_SUCCESS,
    CLDeviceNotFound               = CL_DEVICE_NOT_FOUND,
    CLDeviceNotAvailable           = CL_DEVICE_NOT_AVAILABLE,
    CLCompilerNotAvailable         = CL_COMPILER_NOT_AVAILABLE,
    CLMemObjectAllocationFailure   = CL_MEM_OBJECT_ALLOCATION_FAILURE,
    CLOutOfResources               = CL_OUT_OF_RESOURCES,
    CLOutOfHostMemory              = CL_OUT_OF_HOST_MEMORY,
    CLProfilingInfoNotAvailable    = CL_PROFILING_INFO_NOT_AVAILABLE,
    CLMemCopyOverlap               = CL_MEM_COPY_OVERLAP,
    CLImageFormatMismatch          = CL_IMAGE_FORMAT_MISMATCH,
    CLImageFormatNotSupported      = CL_IMAGE_FORMAT_NOT_SUPPORTED,
    CLBuildProgramFailure          = CL_BUILD_PROGRAM_FAILURE,
    CLMapFailure                   = CL_MAP_FAILURE,
    CLInvalidValue                 = CL_INVALID_VALUE,
    CLInvalidDeviceType            = CL_INVALID_DEVICE_TYPE,
    CLInvalidPlatform              = CL_INVALID_PLATFORM,
    CLInvalidDevice                = CL_INVALID_DEVICE,
    CLInvalidContext               = CL_INVALID_CONTEXT,
    CLInvalidQueueProperties       = CL_INVALID_QUEUE_PROPERTIES,
    CLInvalidCommandQueue          = CL_INVALID_COMMAND_QUEUE,
    CLInvalidHostPtr               = CL_INVALID_HOST_PTR,
    CLInvalidMemObject             = CL_INVALID_MEM_OBJECT,
    CLInvalidImageFormatDescriptor = CL_INVALID_IMAGE_FORMAT_DESCRIPTOR,
    CLInvalidImageSize             = CL_INVALID_IMAGE_SIZE,
    CLInvalidSampler               = CL_INVALID_SAMPLER,
    CLInvalidBinary                = CL_INVALID_BINARY,
    CLInvalidBuildOptions          = CL_INVALID_BUILD_OPTIONS,
    CLInvalidProgram               = CL_INVALID_PROGRAM,
    CLInvalidProgramExecutable     = CL_INVALID_PROGRAM_EXECUTABLE,
    CLInvalidKernelName            = CL_INVALID_KERNEL_NAME,
    CLInvalidKernelDefinition      = CL_INVALID_KERNEL_DEFINITION,
    CLInvalidKernel                = CL_INVALID_KERNEL,
    CLInvalidArgIndex              = CL_INVALID_ARG_INDEX,
    CLInvalidArgValue              = CL_INVALID_ARG_VALUE,
    CLInvalidArgSize               = CL_INVALID_ARG_SIZE,
    CLInvalidKernelArgs            = CL_INVALID_KERNEL_ARGS,
    CLInvalidWorkDimension         = CL_INVALID_WORK_DIMENSION,
    CLInvalidWorkGroupSize         = CL_INVALID_WORK_GROUP_SIZE,
    CLInvalidWorkItemSize          = CL_INVALID_WORK_ITEM_SIZE,
    CLInvalidGlobalOffset          = CL_INVALID_GLOBAL_OFFSET,
    CLInvalidEventWaitList         = CL_INVALID_EVENT_WAIT_LIST,
    CLInvalidEvent                 = CL_INVALID_EVENT,
    CLInvalidOperation             = CL_INVALID_OPERATION,
    CLInvalidGlObject              = CL_INVALID_GL_OBJECT,
    CLInvalidBufferSize            = CL_INVALID_BUFFER_SIZE,
    CLInvalidMipLevel              = CL_INVALID_MIP_LEVEL,
    CLInvalidGlobalWorkSize        = CL_INVALID_GLOBAL_WORK_SIZE,
};

enum CLPlatformInfo {
    CLPlatformProfile    = CL_PLATFORM_PROFILE,
    CLPlatformVersion    = CL_PLATFORM_VERSION,
    CLPlatformName       = CL_PLATFORM_NAME,
    CLPlatformVendor     = CL_PLATFORM_VENDOR,
    CLPlatformExtensions = CL_PLATFORM_EXTENSIONS,
};

enum CLDeviceType {
    CLDeviceTypeCPU         = CL_DEVICE_TYPE_CPU,
    CLDeviceTypeGPU         = CL_DEVICE_TYPE_GPU,
    CLDeviceTypeAccelerator = CL_DEVICE_TYPE_ACCELERATOR,
    CLDeviceTypeDefault     = CL_DEVICE_TYPE_DEFAULT,
    CLDeviceTypeAll         = CL_DEVICE_TYPE_ALL,
};

enum CLDeviceInfo {
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

enum CLContextInfo {
    CLContextReferenceCount = CL_CONTEXT_REFERENCE_COUNT,
    CLContextDevices        = CL_CONTEXT_DEVICES,
    CLContextProperties     = CL_CONTEXT_PROPERTIES,
};
#endc

{#enum CLError                  {} deriving (Eq, Show) #}
{#enum CLPlatformInfo           {} deriving (Eq, Show) #}
{#enum CLDeviceType             {} deriving (Eq, Show) #}
{#enum CLDeviceInfo             {} deriving (Eq, Show) #}
{#enum CLContextInfo            {} deriving (Eq, Show) #}

clError a                       = cFromEnum a
clPlatformInfo a                = cFromEnum a
clDeviceType a                  = cFromEnum a
clDeviceInfo a                  = cFromEnum a
clContextInfo a                 = cFromEnum a