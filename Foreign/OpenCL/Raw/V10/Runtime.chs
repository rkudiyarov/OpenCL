--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

#include <inc_opencl.h>

module Foreign.OpenCL.Raw.V10.Runtime
       ( clCreateCommandQueue
       , clRetainCommandQueue
       , clReleaseCommandQueue
       , clGetCommandQueueInfo

#ifndef CL_VERSION_1_1
       , clSetCommandQueueProperty
#endif

       , clCreateBuffer
       , clEnqueueReadBuffer
       , clEnqueueWriteBuffer
       , clEnqueueCopyBuffer
       , clRetainMemObject
       , clReleaseMemObject
       , clCreateImage2D
       , clCreateImage3D
       , clGetSupportedImageFormats
       , clEnqueueReadImage
       , clEnqueueWriteImage
       , clEnqueueCopyImage
       , clEnqueueCopyImageToBuffer
       , clEnqueueCopyBufferToImage
       , clEnqueueMapBuffer
       , clEnqueueMapImage
       , clEnqueueUnmapMemObject
       , clGetMemObjectInfo
       , clGetImageInfo

       , clCreateSampler
       , clRetainSampler
       , clReleaseSampler
       , clGetSamplerInfo

       , clCreateProgramWithSource
       , clCreateProgramWithBinary
       , clRetainProgram
       , clReleaseProgram
       , clBuildProgram
       , clUnloadCompiler
       , clGetProgramInfo
       , clGetProgramBuildInfo

       , clCreateKernel
       , clCreateKernelsInProgram
       , clRetainKernel
       , clReleaseKernel
       , clSetKernelArg
       , clGetKernelInfo
       , clGetKernelWorkGroupInfo

       , clEnqueueNDRangeKernel
       , clEnqueueTask
       , clEnqueueNativeKernel

       , clWaitForEvents
       , clGetEventInfo
       , clRetainEvent
       , clReleaseEvent

       , clEnqueueMarker
       , clEnqueueWaitForEvents
       , clEnqueueBarrier
       , clGetEventProfilingInfo
       , clFlush
       , clFinish
       )
       where

import Foreign.OpenCL.Raw.C2HS
import Foreign.OpenCL.Raw.V10.Types

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

#ifndef CL_VERSION_1_1
{#fun unsafe clSetCommandQueueProperty as ^
  { cl_command_queue            `CL_command_queue'
  , cl_command_queue_properties `CL_command_queue_properties'
  , cl_bool                     `CL_bool'
  , castPtr                     `Ptr CL_command_queue_properties'
  } -> `CL_int' cl_int
#}
#endif

-- Memory Objects

{#fun unsafe clCreateBuffer as ^
  { cl_context    `CL_context'
  , cl_mem_flags  `CL_mem_flags'
  , cl_size       `CL_size'
  , castPtr       `Ptr a'
  , castPtr       `Ptr CL_int'
  } -> `CL_mem' cl_mem
#}

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

{#fun unsafe clRetainMemObject as ^
  { cl_mem `CL_mem'
  } -> `CL_int' cl_int
#}

{#fun unsafe clReleaseMemObject as ^
  { cl_mem `CL_mem'
  } -> `CL_int' cl_int
#}

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

{#fun unsafe clUnloadCompiler as ^
  { 
  } -> `CL_int' cl_int
#}

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

{#fun unsafe clRetainEvent as ^
  { cl_event `CL_event'
  } -> `CL_int' cl_int
#}

{#fun unsafe clReleaseEvent as ^
  { cl_event `CL_event'
  } -> `CL_int' cl_int
#}

-- Execution of Kernels and Memory Object Commands

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
