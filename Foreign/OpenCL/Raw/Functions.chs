--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2010..2012] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

#include <inc_opencl.h>

module Foreign.OpenCL.Raw.Functions where

import Foreign.OpenCL.Raw.C2HS
import Foreign.OpenCL.Raw.Types

-- Platform

{#fun unsafe clGetPlatformIDs as ^
  { cl_uint `CL_uint'
  , castPtr `Ptr CL_platform_id'
  , castPtr `Ptr CL_uint'
  } -> `CL_int' cl_int
#}

{#fun unsafe clGetPlatformInfo as ^
  { cl_platform_id   `CL_platform_id'
  , cl_platform_info `CL_platform_info'
  , cl_size          `CL_size'
  , castPtr          `Ptr a'
  , castPtr          `Ptr CL_size'
  } -> `CL_int' cl_int
#}

-- Device

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

#ifdef CL_VERSION_1_2
{#fun unsafe clCreateSubDevices as ^
  { cl_device_id `CL_device_id'
  , castPtr      `Ptr CL_device_partition_property'
  , cl_uint      `CL_uint'
  , castPtr      `Ptr CL_device_id'
  , castPtr      `Ptr CL_uint'
  } -> `CL_int' cl_int
#}
#endif

#ifdef CL_VERSION_1_2
{#fun unsafe clRetainDevice as ^
  { cl_device_id `CL_device_id'
  } -> `CL_int' cl_int
#}
#endif

#ifdef CL_VERSION_1_2
{#fun unsafe clReleaseDevice as ^
  { cl_device_id `CL_device_id'
  } -> `CL_int' cl_int
#}
#endif

-- Context

{#fun unsafe clCreateContext as ^
  { castPtr       `Ptr CL_context_properties'
  , cl_uint       `CL_uint'
  , castPtr       `Ptr CL_device_id'
  , castFunPtr    `FunPtr (Ptr CChar -> Ptr () -> CSize -> Ptr () -> IO ())'
  , castPtr       `Ptr a'
  , castPtr       `Ptr CL_int'
  } -> `CL_context' cl_context
#}

{#fun unsafe clCreateContextFromType as ^
  { castPtr        `Ptr CL_context_properties'
  , cl_device_type `CL_device_type'
  , castFunPtr     `FunPtr (Ptr CChar -> Ptr () -> CSize -> Ptr () -> IO ())'
  , castPtr        `Ptr a'
  , castPtr        `Ptr CL_int'
  } -> `CL_context' cl_context
#}

{#fun unsafe clRetainContext as ^
  { cl_context    `CL_context'
  } -> `CL_int' cl_int
#}

{#fun unsafe clReleaseContext as ^
  { cl_context    `CL_context'
  } -> `CL_int' cl_int
#}

{#fun unsafe clGetContextInfo as ^
  { cl_context      `CL_context'
  , cl_context_info `CL_context_info'
  , cl_size         `CL_size'
  , castPtr         `Ptr a'
  , castPtr         `Ptr CL_size'
  } -> `CL_int' cl_int
#}

---- Runtime

-- Command Queues

{#fun unsafe clCreateCommandQueue as ^
  { cl_context                  `CL_context'
  , cl_device_id                `CL_device_id'
  , cl_command_queue_properties `CL_command_queue_properties'
  , castPtr                     `Ptr CL_int'
  } -> `CL_command_queue' cl_command_queue
#}

{#fun unsafe clRetainCommandQueue as ^
  { cl_command_queue `CL_command_queue'
  } -> `CL_int' cl_int
#}

{#fun unsafe clReleaseCommandQueue as ^
  { cl_command_queue `CL_command_queue'
  } -> `CL_int' cl_int
#}

{#fun unsafe clGetCommandQueueInfo as ^
  { cl_command_queue      `CL_command_queue'
  , cl_command_queue_info `CL_command_queue_info'
  , cl_size               `CL_size'
  , castPtr               `Ptr a'
  , castPtr               `Ptr CL_size'
  } -> `CL_int' cl_int
#}

-- Memory Objects

{#fun unsafe clCreateBuffer as ^
  { cl_context    `CL_context'
  , cl_mem_flags  `CL_mem_flags'
  , cl_size       `CL_size'
  , castPtr       `Ptr a'
  , castPtr       `Ptr CL_int'
  } -> `CL_mem' cl_mem
#}

#ifdef CL_VERSION_1_1
{#fun unsafe clCreateSubBuffer as ^
  { cl_mem                `CL_mem'
  , cl_mem_flags          `CL_mem_flags'
  , cl_buffer_create_type `CL_buffer_create_type'
  , castPtr               `Ptr a'
  , castPtr               `Ptr CL_int'
  } -> `CL_mem' cl_mem
#}
#endif

#ifdef CL_VERSION_1_2
{#fun unsafe clCreateImage as ^
  { cl_context   `CL_context'
  , cl_mem_flags `CL_mem_flags'
  , castPtr      `Ptr CL_image_format'
  , castPtr      `Ptr CL_image_desc'
  , castPtr      `Ptr a'
  , castPtr      `Ptr CL_int'
  } -> `CL_mem' cl_mem
#}
#endif

{#fun unsafe clEnqueueReadBuffer as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_bool          `CL_bool'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , castPtr          `Ptr a'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

#ifdef CL_VERSION_1_1
{#fun unsafe clEnqueueReadBufferRect as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_bool          `CL_bool'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , castPtr          `Ptr a'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}
#endif

{#fun unsafe clEnqueueWriteBuffer as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_bool          `CL_bool'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , castPtr          `Ptr a'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

#ifdef CL_VERSION_1_1
{#fun unsafe clEnqueueWriteBufferRect as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_bool          `CL_bool'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , castPtr          `Ptr a'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}
#endif

#ifdef CL_VERSION_1_2
{#fun unsafe clEnqueueFillBuffer as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , castPtr          `Ptr a'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}
#endif

{#fun unsafe clEnqueueCopyBuffer as ^
  { cl_command_queue   `CL_command_queue'
  , cl_mem             `CL_mem'
  , cl_mem             `CL_mem'
  , cl_size            `CL_size'
  , cl_size            `CL_size'
  , cl_size            `CL_size'
  , cl_uint            `CL_uint'
  , castPtr            `Ptr CL_event'
  , castPtr            `Ptr CL_event'
  } -> `CL_int' cl_int
#}

#ifdef CL_VERSION_1_1
{#fun unsafe clEnqueueCopyBufferRect as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_mem           `CL_mem'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}
#endif

{#fun unsafe clRetainMemObject as ^
  { cl_mem `CL_mem'
  } -> `CL_int' cl_int
#}

{#fun unsafe clReleaseMemObject as ^
  { cl_mem `CL_mem'
  } -> `CL_int' cl_int
#}

{#fun unsafe clGetSupportedImageFormats as ^
  { cl_context         `CL_context'
  , cl_mem_flags       `CL_mem_flags'
  , cl_mem_object_type `CL_mem_object_type'
  , cl_uint            `CL_uint'
  , castPtr            `Ptr CL_image_format'
  , castPtr            `Ptr CL_uint'
  } -> `CL_int' cl_int
#}

{#fun unsafe clEnqueueReadImage as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_bool          `CL_bool'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , castPtr          `Ptr a'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

{#fun unsafe clEnqueueWriteImage as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_bool          `CL_bool'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , castPtr          `Ptr a'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

#ifdef CL_VERSION_1_2
{#fun unsafe clEnqueueFillImage as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , castPtr          `Ptr a'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}
#endif

{#fun unsafe clEnqueueCopyImage as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_mem           `CL_mem'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

{#fun unsafe clEnqueueCopyImageToBuffer as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_mem           `CL_mem'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , cl_size          `CL_size'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

{#fun unsafe clEnqueueCopyBufferToImage as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_mem           `CL_mem'
  , cl_size          `CL_size'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

{#fun unsafe clEnqueueMapBuffer as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_bool          `CL_bool'
  , cl_map_flags     `CL_map_flags'
  , cl_size          `CL_size'
  , cl_size          `CL_size'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_int'
  } -> `Ptr a' castPtr
#}

{#fun unsafe clEnqueueMapImage as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , cl_bool          `CL_bool'
  , cl_map_flags     `CL_map_flags'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_int'
  } -> `Ptr a' castPtr
#}

{#fun unsafe clEnqueueUnmapMemObject as ^
  { cl_command_queue `CL_command_queue'
  , cl_mem           `CL_mem'
  , castPtr          `Ptr a'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

#ifdef CL_VERSION_1_2
{#fun unsafe clEnqueueMigrateMemObjects as ^
  { cl_command_queue       `CL_command_queue'
  , cl_uint                `CL_uint'
  , castPtr                `Ptr cl_mem'
  , cl_mem_migration_flags `CL_mem_migration_flags'
  , cl_uint                `CL_uint'
  , castPtr                `Ptr CL_event'
  , castPtr                `Ptr CL_event'
  } -> `CL_int' cl_int
#}
#endif

{#fun unsafe clGetMemObjectInfo as ^
  { cl_mem        `CL_mem'
  , cl_mem_info   `CL_mem_info'
  , cl_size       `CL_size'
  , castPtr       `Ptr a'
  , castPtr       `Ptr CL_size'
  } -> `CL_int' cl_int
#}

{#fun unsafe clGetImageInfo as ^
  { cl_mem        `CL_mem'
  , cl_image_info `CL_image_info'
  , cl_size       `CL_size'
  , castPtr       `Ptr a'
  , castPtr       `Ptr CL_size'
  } -> `CL_int' cl_int
#}

#ifdef CL_VERSION_1_1
{#fun unsafe clSetMemObjectDestructorCallback as ^
  { cl_mem     `CL_mem'
  , castFunPtr `FunPtr (CL_mem -> Ptr a -> IO ())'
  , castPtr    `Ptr a'
  } -> `CL_int' cl_int
#}
#endif

-- Sampler Objects

{#fun unsafe clCreateSampler as ^
  { cl_context         `CL_context'
  , cl_bool            `CL_bool'
  , cl_addressing_mode `CL_addressing_mode'
  , cl_filter_mode     `CL_filter_mode'
  , castPtr            `Ptr CL_int'
  } -> `CL_sampler' cl_sampler
#}

{#fun unsafe clRetainSampler as ^
  { cl_sampler `CL_sampler'
  } -> `CL_int' cl_int
#}

{#fun unsafe clReleaseSampler as ^
  { cl_sampler `CL_sampler'
  } -> `CL_int' cl_int
#}

{#fun unsafe clGetSamplerInfo as ^
  { cl_sampler      `CL_sampler'
  , cl_sampler_info `CL_sampler_info'
  , cl_size         `CL_size'
  , castPtr         `Ptr a'
  , castPtr         `Ptr CL_size'
  } -> `CL_int' cl_int
#}

-- Program Objects

{#fun unsafe clCreateProgramWithSource as ^
  { cl_context `CL_context'
  , cl_uint    `CL_uint'
  , castPtr    `Ptr (Ptr CChar)'
  , castPtr    `Ptr CL_size'
  , castPtr    `Ptr CL_int'
  } -> `CL_program' cl_program
#}

{#fun unsafe clCreateProgramWithBinary as ^
  { cl_context `CL_context'
  , cl_uint    `CL_uint'
  , castPtr    `Ptr CL_device_id'
  , castPtr    `Ptr CL_size'
  , castPtr    `Ptr (Ptr CUChar)'
  , castPtr    `Ptr CL_int'
  , castPtr    `Ptr CL_int'
  } -> `CL_program' cl_program
#}

#ifdef CL_VERSION_1_2
{#fun unsafe clCreateProgramWithBuiltInKernels as ^
  { cl_context `CL_context'
  , cl_uint    `CL_uint'
  , castPtr    `Ptr CL_device_id'
  , castPtr    `Ptr CChar'
  , castPtr    `Ptr CL_int'
  } -> `CL_program' cl_program
#}
#endif

{#fun unsafe clRetainProgram as ^
  { cl_program `CL_program'
  } -> `CL_int' cl_int
#}

{#fun unsafe clReleaseProgram as ^
  { cl_program `CL_program'
  } -> `CL_int' cl_int
#}

{#fun unsafe clBuildProgram as ^
  { cl_program `CL_program'
  , cl_uint    `CL_uint'
  , castPtr    `Ptr CL_device_id'
  , castPtr    `Ptr CChar'
  , castFunPtr `FunPtr (CL_program -> Ptr a -> IO ())'
  , castPtr    `Ptr a'
  } -> `CL_int' cl_int
#}

#ifdef CL_VERSION_1_2
{#fun unsafe clCompileProgram as ^
  { cl_program `CL_program'
  , cl_uint    `CL_uint'
  , castPtr    `Ptr CL_device_id'
  , castPtr    `Ptr CChar'
  , cl_uint    `CL_uint'
  , castPtr    `Ptr CL_program'
  , castPtr    `Ptr (Ptr CChar)'
  , castFunPtr `FunPtr (CL_program -> Ptr a -> IO ())'
  , castPtr    `Ptr a'
  } -> `CL_int' cl_int
#}
#endif

#ifdef CL_VERSION_1_2
{#fun unsafe clLinkProgram as ^
  { cl_context `CL_context'
  , cl_uint    `CL_uint'
  , castPtr    `Ptr CL_device_id'
  , castPtr    `Ptr CChar'
  , cl_uint    `CL_uint'
  , castPtr    `Ptr CL_program'
  , castFunPtr `FunPtr (CL_program -> Ptr a -> IO ())'
  , castPtr    `Ptr a'
  , castPtr    `Ptr CL_int'
  } -> `CL_program' cl_program
#}
#endif

#ifdef CL_VERSION_1_2
{#fun unsafe clUnloadPlatformCompiler as ^
  { cl_platform_id `CL_platform_id'
  } -> `CL_int' cl_int
#}
#endif

{#fun unsafe clGetProgramInfo as ^
  { cl_program      `CL_program'
  , cl_program_info `CL_program_info'
  , cl_size         `CL_size'
  , castPtr         `Ptr a'
  , castPtr         `Ptr CL_size'
  } -> `CL_int' cl_int
#}

{#fun unsafe clGetProgramBuildInfo as ^
  { cl_program            `CL_program'
  , cl_device_id          `CL_device_id'
  , cl_program_build_info `CL_program_build_info'
  , cl_size               `CL_size'
  , castPtr               `Ptr a'
  , castPtr               `Ptr CL_size'
  } -> `CL_int' cl_int
#}

-- Kernel Objects

{#fun unsafe clCreateKernel as ^
  { cl_program `CL_program'
  , castPtr    `Ptr CChar'
  , castPtr    `Ptr CL_int'
  } -> `CL_kernel' cl_kernel
#}

{#fun unsafe clCreateKernelsInProgram as ^
  { cl_program `CL_program'
  , cl_uint    `CL_uint'
  , castPtr    `Ptr CL_kernel'
  , castPtr    `Ptr CL_uint'
  } -> `CL_int' cl_int
#}

{#fun unsafe clRetainKernel as ^
  { cl_kernel `CL_kernel'
  } -> `CL_int' cl_int
#}

{#fun unsafe clReleaseKernel as ^
  { cl_kernel `CL_kernel'
  } -> `CL_int' cl_int
#}

{#fun unsafe clSetKernelArg as ^
  { cl_kernel `CL_kernel'
  , cl_uint   `CL_uint'
  , cl_size   `CL_size'
  , castPtr   `Ptr a'
  } -> `CL_int' cl_int
#}

{#fun unsafe clGetKernelInfo as ^
  { cl_kernel      `CL_kernel'
  , cl_kernel_info `CL_kernel_info'
  , cl_size        `CL_size'
  , castPtr        `Ptr a'
  , castPtr        `Ptr CL_size'
  } -> `CL_int' cl_int
#}

#ifdef CL_VERSION_1_2
{#fun unsafe clGetKernelArgInfo as ^
  { cl_kernel          `CL_kernel'
  , cl_uint            `CL_uint'
  , cl_kernel_arg_info `CL_kernel_arg_info'
  , cl_size            `CL_size'
  , castPtr            `Ptr a'
  , castPtr            `Ptr CL_size'
  } -> `CL_int' cl_int
#}
#endif

{#fun unsafe clGetKernelWorkGroupInfo as ^
  { cl_kernel                 `CL_kernel'
  , cl_device_id              `CL_device_id'
  , cl_kernel_work_group_info `CL_kernel_work_group_info'
  , cl_size                   `CL_size'
  , castPtr                   `Ptr a'
  , castPtr                   `Ptr CL_size'
  } -> `CL_int' cl_int
#}

-- Executing Kernels

{#fun unsafe clEnqueueNDRangeKernel as ^
  { cl_command_queue `CL_command_queue'
  , cl_kernel        `CL_kernel'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , castPtr          `Ptr CL_size'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

{#fun unsafe clEnqueueTask as ^
  { cl_command_queue `CL_command_queue'
  , cl_kernel        `CL_kernel'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

{#fun unsafe clEnqueueNativeKernel as ^
  { cl_command_queue `CL_command_queue'
  , castFunPtr       `FunPtr (Ptr a -> IO ())'
  , castPtr          `Ptr a'
  , cl_size          `CL_size'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_mem'
  , castPtr          `Ptr (Ptr a)'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

#ifdef CL_VERSION_1_2
{#fun unsafe clEnqueueMarkerWithWaitList as ^
  { cl_command_queue `CL_command_queue'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}
#endif

#ifdef CL_VERSION_1_2
{#fun unsafe clEnqueueBarrierWithWaitList as ^
  { cl_command_queue `CL_command_queue'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}
#endif

#ifdef CL_VERSION_1_2
{#fun unsafe clSetPrintfCallback as ^
  { cl_context `CL_context'
  , castFunPtr `FunPtr (CL_context -> CL_uint -> Ptr CChar -> Ptr a -> IO())'
  , castPtr    `Ptr a'
  } -> `CL_int' cl_int
#}
#endif

-- Event Objects

{#fun unsafe clWaitForEvents as ^
  { cl_uint `CL_uint'
  , castPtr `Ptr CL_event'
  } -> `CL_int' cl_int
#}

{#fun unsafe clGetEventInfo as ^
  { cl_event      `CL_event'
  , cl_event_info `CL_event_info'
  , cl_size       `CL_size'
  , castPtr       `Ptr a'
  , castPtr       `Ptr CL_size'
  } -> `CL_int' cl_int
#}

#ifdef CL_VERSION_1_1
{#fun unsafe clCreateUserEvent as ^
  { cl_context `CL_context'
  , castPtr    `Ptr a'
  } -> `CL_event' cl_event
#}
#endif

{#fun unsafe clRetainEvent as ^
  { cl_event `CL_event'
  } -> `CL_int' cl_int
#}

{#fun unsafe clReleaseEvent as ^
  { cl_event `CL_event'
  } -> `CL_int' cl_int
#}

#ifdef CL_VERSION_1_1
{#fun unsafe clSetUserEventStatus as ^
  { cl_event `CL_event'
  , cl_int   `CL_int'
  } -> `CL_int' cl_int
#}
#endif

#ifdef CL_VERSION_1_1
{#fun unsafe clSetEventCallback as ^
  { cl_event   `CL_event'
  , cl_int     `CL_int'
  , castFunPtr `FunPtr (CL_event -> CL_int -> Ptr a -> IO ())'
  , castPtr    `Ptr a'
  } -> `CL_int' cl_int
#}
#endif

-- Execution of Kernels and Memory Object Commands

-- Profiling Operations on Memory Objects and Kernels

{#fun unsafe clGetEventProfilingInfo as ^
  { cl_event          `CL_event'
  , cl_profiling_info `CL_profiling_info'
  , cl_size           `CL_size'
  , castPtr           `Ptr a'
  , castPtr           `Ptr CL_size'
  } -> `CL_int' cl_int
#}

-- Flush and Finish

{#fun unsafe clFlush as ^
  { cl_command_queue `CL_command_queue'
  } -> `CL_int' cl_int
#}

{#fun unsafe clFinish as ^
  { cl_command_queue `CL_command_queue'
  } -> `CL_int' cl_int
#}

---- Deprecated

#ifndef CL_VERSION_1_1

{#fun unsafe clSetCommandQueueProperty as ^
  { cl_command_queue            `CL_command_queue'
  , cl_command_queue_properties `CL_command_queue_properties'
  , cl_bool                     `CL_bool'
  , castPtr                     `Ptr CL_command_queue_properties'
  } -> `CL_int' cl_int
#}

#endif

#ifndef CL_VERSION_1_2

{#fun unsafe clCreateImage2D as ^
  { cl_context   `CL_context'
  , cl_mem_flags `CL_mem_flags'
  , castPtr      `Ptr CL_image_format'
  , cl_size      `CL_size'
  , cl_size      `CL_size'
  , cl_size      `CL_size'
  , castPtr      `Ptr a'
  , castPtr      `Ptr CL_int'
  } -> `CL_mem' cl_mem
#}

{#fun unsafe clCreateImage3D as ^
  { cl_context   `CL_context'
  , cl_mem_flags `CL_mem_flags'
  , castPtr      `Ptr CL_image_format'
  , cl_size      `CL_size'
  , cl_size      `CL_size'
  , cl_size      `CL_size'
  , cl_size      `CL_size'
  , cl_size      `CL_size'
  , castPtr      `Ptr a'
  , castPtr      `Ptr CL_int'
  } -> `CL_mem' cl_mem
#}

{#fun unsafe clEnqueueMarker as ^
  { cl_command_queue `CL_command_queue'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

{#fun unsafe clEnqueueWaitForEvents as ^
  { cl_command_queue `CL_command_queue'
  , cl_uint          `CL_uint'
  , castPtr          `Ptr CL_event'
  } -> `CL_int' cl_int
#}

{#fun unsafe clEnqueueBarrier as ^
  { cl_command_queue `CL_command_queue'
  } -> `CL_int' cl_int
#}

{#fun unsafe clUnloadCompiler as ^
  {
  } -> `CL_int' cl_int
#}

#endif
