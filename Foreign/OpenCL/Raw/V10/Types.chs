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

type CLChar = {# type cl_char #}
cl_char = cIntConv
type CLUChar = {# type cl_uchar #}
cl_uchar = cIntConv
type CLShort = {# type cl_short #}
cl_short = cIntConv
type CLUShort = {# type cl_ushort #}
cl_ushort = cIntConv
type CLInt = {# type cl_int #}
cl_int = cIntConv
type CLUInt = {# type cl_uint #}
cl_uint = cIntConv
type CLLong = {# type cl_long #}
cl_long = cIntConv
type CLULong = {# type cl_ulong #}
cl_ulong = cIntConv
type CLHalf = {# type cl_half #}
cl_half = cIntConv
type CLFloat = {# type cl_float #}
cl_float = cFloatConv
type CLDouble = {# type cl_double #}
cl_double = cFloatConv

type CLPlatformID = {# type cl_platform_id #}
cl_platform_id = castPtr
type CLDeviceID = {# type cl_device_id #}
cl_device_id = castPtr
type CLContext = {# type cl_context #}
cl_context = castPtr
type CLCommandQueue = {# type cl_command_queue #}
cl_command_queue = castPtr
type CLMem = {# type cl_mem #}
cl_mem = castPtr
type CLProgram = {# type cl_program #}
cl_program = castPtr
type CLKernet = {# type cl_kernel #}
cl_kernel = castPtr
type CLEvent = {# type cl_event #}
cl_event = castPtr
type CLSampler = {# type cl_sampler #}
cl_sampler = castPtr

type CLBool = {# type cl_bool #}
cl_bool = cl_uint
type CLBitfield = {# type cl_bitfield #}
cl_bitfield = cl_ulong
type CLDeviceType = {# type cl_device_type #}
cl_device_type = cl_bitfield
type CLPlatformInfo = {# type cl_platform_info #}
cl_platform_info = cl_uint
type CLDeviceInfo = {# type cl_device_info #}
cl_device_info = cl_uint
type CLDeviceAddressInfo = {# type cl_device_address_info #}
cl_device_address_info = cl_bitfield
type CLDeviceFpConfig = {# type cl_device_fp_config #}
cl_device_fp_config = cl_bitfield
type CLDeviceMemCacheType = {# type cl_device_mem_cache_type #}
cl_device_mem_cache_type = cl_uint
type CLDeviceLocalMemType = {# type cl_device_local_mem_type #}
cl_device_local_mem_type = cl_uint
type CLDeviceExecCapabilities = {# type cl_device_exec_capabilities #}
cl_device_exec_capabilities = cl_bitfield
type CLCommandQueueProperties = {# type cl_command_queue_properties #}
cl_command_queue_properties = cl_bitfield

type CLContextPreperties = {# type cl_context_properties #}
cl_context_properties = castPtr
type CLContextInfo = {# type cl_context_info #}
cl_context_info = cl_uint
type CLCommandQueueInfo = {# type cl_command_queue_info #}
cl_command_queue_info = cl_uint
type CLChannelOrder = {# type cl_channel_order #}
cl_channel_order = cl_uint
type CLChannelType = {# type cl_channel_type #}
cl_channel_type = cl_uint
type CLMemFlags = {# type cl_mem_flags #}
cl_mem_flags = cl_bifield
type CLMemObjectType = {# type cl_mem_object_type #}
cl_mem_object_type = cl_uint
type CLMemInfo = {# type cl_mem_info #}
cl_mem_info = cl_uint
type CLImageInfo = {# type cl_image_info #}
cl_image_info = cl_uint
type CLAddressingMode = {# type cl_addressing_mode #}
cl_addressing_mode = cl_uint
type CLFilterMode = {# type cl_filter_mode #}
cl_filter_mode = cl_uint
type CLSamplerInfo = {# type cl_sampler_info #}
cl_sampler_info = cl_uint
type CLMapFlags = {# type cl_map_flags #}
cl_map_flags = cl_bitfield
type CLProgramInfo = {# type cl_program_info #}
cl_program_info = cl_uint
type CLProgramBuildInfo = {# type cl_program_build_info #}
cl_program_build_info = cl_uint
type CLBuildStatus = {# type cl_build_status #}
cl_build_status = cl_int
type CLKernelInfo = {# type cl_kernel_info #}
cl_kernel_info = cl_uint
type CLKernelWorkGroupInfo = {# type cl_kernel_work_group_info #}
cl_kernel_work_group_info = cl_uint
type CLEventInfo = {# type cl_event_info #}
cl_event_info = cl_uint
type CLCommandType = {# type cl_command_type #}
cl_command_type = cl_uint
type CLProfilingInfo = {# type cl_profiling_info #}
cl_profiling_info = cl_uint
--type CLImageFormat = {# type cl_image_format #}

