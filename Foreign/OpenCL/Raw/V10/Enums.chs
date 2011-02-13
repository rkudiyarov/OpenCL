--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Enums where

#include <inc_opencl.h>

import Foreign.OpenCL.Raw.C2HS

#c
enum CLVersion {
    CLVersion10 = CL_VERSION_1_0,
};

enum CLBool {
    CLFalse = CL_FALSE,
    CLTrue  = CL_TRUE,
};

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

enum CLDeviceFpConfig {
    CLFpDenorm         = CL_FP_DENORM,
    CLFpInfNan         = CL_FP_INF_NAN,
    CLFpRoundToNearest = CL_FP_ROUND_TO_NEAREST,
    CLFpRoundToZero    = CL_FP_ROUND_TO_ZERO,
    CLFpRoundToInf     = CL_FP_ROUND_TO_INF,
    CLFpFma            = CL_FP_FMA,
};

enum CLDeviceMemCacheType {
    CLNone           = CL_NONE,
    CLReadOnlyCache  = CL_READ_ONLY_CACHE,
    CLReadWriteCache = CL_READ_WRITE_CACHE,
};

enum CLDeviceLocalMemType {
    CLLocal  = CL_LOCAL,
    CLGlobal = CL_GLOBAL,
};

enum CLDeviceExecCapabilities {
    CLExecKernel       = CL_EXEC_KERNEL,
    CLExecNativeKernel = CL_EXEC_NATIVE_KERNEL,
};

enum CLContextProperties {
    CLContextPlatform = CL_CONTEXT_PLATFORM,
};

enum CLContextInfo {
    CLContextReferenceCount = CL_CONTEXT_REFERENCE_COUNT,
    CLContextDevices        = CL_CONTEXT_DEVICES,
    CLContextProperties     = CL_CONTEXT_PROPERTIES,
};

enum CLCommandQueueProperties {
    CLQueueOutOfOrderExecModeEnable = CL_QUEUE_OUT_OF_ORDER_EXEC_MODE_ENABLE,
    CLQueueProfilingEnable          = CL_QUEUE_PROFILING_ENABLE,
};

enum CLCommandQueueInfo {
    CLQueueContext        = CL_QUEUE_CONTEXT,
    CLQueueDevice         = CL_QUEUE_DEVICE,
    CLQueueReferenceCount = CL_QUEUE_REFERENCE_COUNT,
    CLQueueProperties     = CL_QUEUE_PROPERTIES,
};

enum CLMemFlags {
    CLMemReadWrite    = CL_MEM_READ_WRITE,
    CLMemWriteOnly    = CL_MEM_WRITE_ONLY,
    CLMemReadOnly     = CL_MEM_READ_ONLY,
    CLMemUseHostPtr   = CL_MEM_USE_HOST_PTR,
    CLMemAllocHostPtr = CL_MEM_ALLOC_HOST_PTR,
    CLMemCopyHostPtr  = CL_MEM_COPY_HOST_PTR,
};

enum CLChannelOrder {
    CLR         = CL_R,
    CLA         = CL_A,
    CLIntensity = CL_INTENSITY,
    CLLuminance = CL_LUMINANCE,
    CLRG        = CL_RG,
    CLRA        = CL_RA,
    CLRGB       = CL_RGB,
    CLRGBA      = CL_RGBA,
    CLBGRA      = CL_BGRA,
    CLARGB      = CL_ARGB,
};

enum CLChannelType {
    CLSnormInt8      = CL_SNORM_INT8,
    CLSnormInt16     = CL_SNORM_INT16,
    CLUnormInt8      = CL_UNORM_INT8,
    CLUnormInt16     = CL_UNORM_INT16,
    CLUnormShort565  = CL_UNORM_SHORT_565,
    CLUnormShort555  = CL_UNORM_SHORT_555,
    CLUnormInt101010 = CL_UNORM_INT_101010,
    CLSignedInt8     = CL_SIGNED_INT8,
    CLSignedInt16    = CL_SIGNED_INT16,
    CLSignedInt32    = CL_SIGNED_INT32,
    CLUnsignedInt8   = CL_UNSIGNED_INT8,
    CLUnsignedInt16  = CL_UNSIGNED_INT16,
    CLUnsignedInt32  = CL_UNSIGNED_INT32,
    CLHalfFloat      = CL_HALF_FLOAT,
    CLFloat          = CL_FLOAT,
};

enum CLMapFlags {
    CLMapRead  = CL_MAP_READ,
    CLMapWrite = CL_MAP_WRITE,
};

enum CLMemInfo {
    CLMemType           = CL_MEM_TYPE,
    CLMemFlags          = CL_MEM_FLAGS,
    CLMemSize           = CL_MEM_SIZE,
    CLMemHostPtr        = CL_MEM_HOST_PTR,
    CLMemMapCount       = CL_MEM_MAP_COUNT,
    CLMemReferenceCount = CL_MEM_REFERENCE_COUNT,
    CLMemContext        = CL_MEM_CONTEXT,
};

enum CLMemObjectType {
    CLMemObjectBuffer  = CL_MEM_OBJECT_BUFFER,
    CLMemObjectImage2d = CL_MEM_OBJECT_IMAGE2D,
    CLMemObjectImage3d = CL_MEM_OBJECT_IMAGE3D,
};

enum CLImageInfo {
    CLImageFormat      = CL_IMAGE_FORMAT,
    CLImageElementSize = CL_IMAGE_ELEMENT_SIZE,
    CLImageRowPitch    = CL_IMAGE_ROW_PITCH,
    CLImageSlicePitch  = CL_IMAGE_SLICE_PITCH,
    CLImageWidth       = CL_IMAGE_WIDTH,
    CLImageHeight      = CL_IMAGE_HEIGHT,
    CLImageDepth       = CL_IMAGE_DEPTH,
};

enum CLAddressingMode {
    CLAddressNone        = CL_ADDRESS_NONE,
    CLAddressClampToEdge = CL_ADDRESS_CLAMP_TO_EDGE,
    CLAddressClamp       = CL_ADDRESS_CLAMP,
    CLAddressRepeat      = CL_ADDRESS_REPEAT,
};

enum CLFilteringMode {
    CLFilterNearest = CL_FILTER_NEAREST,
    CLFilterLinear  = CL_FILTER_LINEAR,
};
enum CLSamplerInfo {
    CLSamplerReferenceCount   = CL_SAMPLER_REFERENCE_COUNT,
    CLSamplerContext          = CL_SAMPLER_CONTEXT,
    CLSamplerNormalizedCoords = CL_SAMPLER_NORMALIZED_COORDS,
    CLSamplerAddressingMode   = CL_SAMPLER_ADDRESSING_MODE,
    CLSamplerFilterMode       = CL_SAMPLER_FILTER_MODE,
};

enum CLProgramInfo {
    CLProgramReferenceCount = CL_PROGRAM_REFERENCE_COUNT,
    CLProgramContext        = CL_PROGRAM_CONTEXT,
    CLProgramNumDevices     = CL_PROGRAM_NUM_DEVICES,
    CLProgramDevices        = CL_PROGRAM_DEVICES,
    CLProgramSource         = CL_PROGRAM_SOURCE,
    CLProgramBinarySizes    = CL_PROGRAM_BINARY_SIZES,
    CLProgramBinaries       = CL_PROGRAM_BINARIES,
};

enum CLProgramBuildInfo {
    CLProgramBuildStatus  = CL_PROGRAM_BUILD_STATUS,
    CLProgramBuildOptions = CL_PROGRAM_BUILD_OPTIONS,
    CLProgramBuildLog     = CL_PROGRAM_BUILD_LOG,
};

enum CLBuildStatus {
    CLBuildSuccess    = CL_BUILD_SUCCESS,
    CLBuildNone       = CL_BUILD_NONE,
    CLBuildError      = CL_BUILD_ERROR,
    CLBuildInProgress = CL_BUILD_IN_PROGRESS,
};

enum CLKernelInfo {
    CLKernelFunctionName   = CL_KERNEL_FUNCTION_NAME,
    CLKernelNumArgs        = CL_KERNEL_NUM_ARGS,
    CLKernelReferenceCount = CL_KERNEL_REFERENCE_COUNT,
    CLKernelContext        = CL_KERNEL_CONTEXT,
    CLKernelProgram        = CL_KERNEL_PROGRAM,
};

enum CLKernelWorkGroupInfo {
    CLKernelWorkGroupSize        = CL_KERNEL_WORK_GROUP_SIZE,
    CLKernelCompileWorkGroupSize = CL_KERNEL_COMPILE_WORK_GROUP_SIZE,
    CLKernelLocalMemSize         = CL_KERNEL_LOCAL_MEM_SIZE,
};

enum CLEventInfo {
    CLEventCommandQueue           = CL_EVENT_COMMAND_QUEUE,
    CLEventCommandType            = CL_EVENT_COMMAND_TYPE,
    CLEventReferenceCount         = CL_EVENT_REFERENCE_COUNT,
    CLEventCommandExecutionStatus = CL_EVENT_COMMAND_EXECUTION_STATUS,
};

enum CLExecutionStatus {
    CLComplete  = CL_COMPLETE,
    CLRunning   = CL_RUNNING,
    CLSubmitted = CL_SUBMITTED,
    CLQueued    = CL_QUEUED,
};

enum CLCommandType {
    CLCommandNdrangeKernel     = CL_COMMAND_NDRANGE_KERNEL,
    CLCommandTask              = CL_COMMAND_TASK,
    CLCommandNativeKernel      = CL_COMMAND_NATIVE_KERNEL,
    CLCommandReadBuffer        = CL_COMMAND_READ_BUFFER,
    CLCommandWriteBuffer       = CL_COMMAND_WRITE_BUFFER,
    CLCommandCopyBuffer        = CL_COMMAND_COPY_BUFFER,
    CLCommandReadImage         = CL_COMMAND_READ_IMAGE,
    CLCommandWriteImage        = CL_COMMAND_WRITE_IMAGE,
    CLCommandCopyImage         = CL_COMMAND_COPY_IMAGE,
    CLCommandCopyImageToBuffer = CL_COMMAND_COPY_IMAGE_TO_BUFFER,
    CLCommandCopyBufferToImage = CL_COMMAND_COPY_BUFFER_TO_IMAGE,
    CLCommandMapBuffer         = CL_COMMAND_MAP_BUFFER,
    CLCommandMapImage          = CL_COMMAND_MAP_IMAGE,
    CLCommandUnmapMemObject    = CL_COMMAND_UNMAP_MEM_OBJECT,
    CLCommandMarker            = CL_COMMAND_MARKER,
    CLCommandAcquireGlObjects  = CL_COMMAND_ACQUIRE_GL_OBJECTS,
    CLCommandReleaseGlObjects  = CL_COMMAND_RELEASE_GL_OBJECTS,
};

enum CLProfilingInfo {
    CLProfilingCommandQueued = CL_PROFILING_COMMAND_QUEUED,
    CLProfilingCommandSubmit = CL_PROFILING_COMMAND_SUBMIT,
    CLProfilingCommandStart  = CL_PROFILING_COMMAND_START,
    CLProfilingCommandEnd    = CL_PROFILING_COMMAND_END,
};
#endc

{#enum CLVersion                {} deriving (Eq, Show) #}
{#enum CLBool                   {} deriving (Eq, Show) #}
{#enum CLError                  {} deriving (Eq, Show) #}
{#enum CLPlatformInfo           {} deriving (Eq, Show) #}
{#enum CLDeviceType             {} deriving (Eq, Show) #}
{#enum CLDeviceInfo             {} deriving (Eq, Show) #}
{#enum CLDeviceFpConfig         {} deriving (Eq, Show) #}
{#enum CLDeviceMemCacheType     {} deriving (Eq, Show) #}
{#enum CLDeviceLocalMemType     {} deriving (Eq, Show) #}
{#enum CLDeviceExecCapabilities {} deriving (Eq, Show) #}
{#enum CLContextProperties      {} deriving (Eq, Show) #}
{#enum CLContextInfo            {} deriving (Eq, Show) #}
{#enum CLCommandQueueProperties {} deriving (Eq, Show) #}
{#enum CLCommandQueueInfo       {} deriving (Eq, Show) #}
{#enum CLMemFlags               {} deriving (Eq, Show) #}
{#enum CLChannelOrder           {} deriving (Eq, Show) #}
{#enum CLChannelType            {} deriving (Eq, Show) #}
{#enum CLMapFlags               {} deriving (Eq, Show) #}
{#enum CLMemInfo                {} deriving (Eq, Show) #}
{#enum CLMemObjectType          {} deriving (Eq, Show) #}
{#enum CLImageInfo              {} deriving (Eq, Show) #}
{#enum CLAddressingMode         {} deriving (Eq, Show) #}
{#enum CLFilteringMode          {} deriving (Eq, Show) #}
{#enum CLSamplerInfo            {} deriving (Eq, Show) #}
{#enum CLProgramInfo            {} deriving (Eq, Show) #}
{#enum CLProgramBuildInfo       {} deriving (Eq, Show) #}
{#enum CLBuildStatus            {} deriving (Eq, Show) #}
{#enum CLKernelInfo             {} deriving (Eq, Show) #}
{#enum CLKernelWorkGroupInfo    {} deriving (Eq, Show) #}
{#enum CLEventInfo              {} deriving (Eq, Show) #}
{#enum CLExecutionStatus        {} deriving (Eq, Show) #}
{#enum CLCommandType            {} deriving (Eq, Show) #}
{#enum CLProfilingInfo          {} deriving (Eq, Show) #}
