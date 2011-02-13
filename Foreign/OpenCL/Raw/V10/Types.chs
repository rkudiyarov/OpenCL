--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2010..2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Types where

#include <inc_opencl.h>

import Foreign
import Foreign.C
import Foreign.OpenCL.Raw.C2HS

type CL_size     = {# type size_t #}
cl_size a        = cIntConv a
type CL_char     = {# type cl_char #}
cl_char a        = cIntConv a
type CL_uchar    = {# type cl_uchar #}
cl_uchar a       = cIntConv a
type CL_short    = {# type cl_short #}
cl_short a       = cIntConv a
type CL_ushort   = {# type cl_ushort #}
cl_ushort a      = cIntConv a
type CL_int      = {# type cl_int #}
cl_int a         = cIntConv a
type CL_uint     = {# type cl_uint #}
cl_uint a        = cIntConv a
type CL_long     = {# type cl_long #}
cl_long a        = cIntConv a
type CL_ulong    = {# type cl_ulong #}
cl_ulong a       = cIntConv a
type CL_half     = {# type cl_half #}
cl_half a        = cIntConv a
type CL_float    = {# type cl_float #}
cl_float a       = cFloatConv a
type CL_double   = {# type cl_double #}
cl_double a      = cFloatConv a
type CL_bool     = {# type cl_bool #}
cl_bool a        = cl_uint a
type CL_bitfield = {# type cl_bitfield #}
cl_bitfield a    = cl_ulong a

type CL_platform_id   = {# type cl_platform_id #}
cl_platform_id a      = castPtr a
type CL_platform_info = {# type cl_platform_info #}
cl_platform_info a    = cl_uint a

type CL_device_id                = {# type cl_device_id #}
cl_device_id a                   = castPtr a
type CL_device_type              = {# type cl_device_type #}
cl_device_type a                 = cl_bitfield a
type CL_device_info              = {# type cl_device_info #}
cl_device_info a                 = cl_uint a
type CL_device_fp_config         = {# type cl_device_fp_config #}
cl_device_fp_config a            = cl_bitfield a
type CL_device_mem_cache_type    = {# type cl_device_mem_cache_type #}
cl_device_mem_cache_type a       = cl_uint a
type CL_device_local_mem_type    = {# type cl_device_local_mem_type #}
cl_device_local_mem_type a       = cl_uint a
type CL_device_exec_capabilities = {# type cl_device_exec_capabilities #}
cl_device_exec_capabilities a    = cl_bitfield a

type CL_context            = {# type cl_context #}
cl_context a               = castPtr a
type CL_context_properties = {# type cl_context_properties #}
cl_context_properties a    = castPtr a
type CL_context_info       = {# type cl_context_info #}
cl_context_info a          = cl_uint a

type CL_command_queue            = {# type cl_command_queue #}
cl_command_queue a               = castPtr a
type CL_command_queue_properties = {# type cl_command_queue_properties #}
cl_command_queue_properties a    = cl_bitfield a
type CL_command_queue_info       = {# type cl_command_queue_info #}
cl_command_queue_info a          = cl_uint a

type CL_channel_order = {# type cl_channel_order #}
cl_channel_order a    = cl_uint a
type CL_channel_type  = {# type cl_channel_type #}
cl_channel_type a     = cl_uint a
type CL_image_info    = {# type cl_image_info #}
cl_image_info a       = cl_uint a

type CL_map_flags = {# type cl_map_flags #}
cl_map_flags a    = cl_bitfield a

type CL_mem             = {# type cl_mem #}
cl_mem a                = castPtr a
type CL_mem_flags       = {# type cl_mem_flags #}
cl_mem_flags a          = cl_bitfield a
type CL_mem_object_type = {# type cl_mem_object_type #}
cl_mem_object_type a    = cl_uint a
type CL_mem_info        = {# type cl_mem_info #}
cl_mem_info a           = cl_uint a

type CL_event        = {# type cl_event #}
cl_event a           = castPtr a
type CL_event_info   = {# type cl_event_info #}
cl_event_info a      = cl_uint a
type CL_command_type = {# type cl_command_type #}
cl_command_type a    = cl_uint a

type CL_sampler         = {# type cl_sampler #}
cl_sampler a            = castPtr a
type CL_sampler_info    = {# type cl_sampler_info #}
cl_sampler_info a       = cl_uint a
type CL_addressing_mode = {# type cl_addressing_mode #}
cl_addressing_mode a    = cl_uint a
type CL_filter_mode     = {# type cl_filter_mode #}
cl_filter_mode a        = cl_uint a

type CL_program            = {# type cl_program #}
cl_program a               = castPtr a
type CL_program_info       = {# type cl_program_info #}
cl_program_info a          = cl_uint a
type CL_program_build_info = {# type cl_program_build_info #}
cl_program_build_info a    = cl_uint a
type CL_build_status       = {# type cl_build_status #}
cl_build_status a          = cl_int a

type CL_kernel                 = {# type cl_kernel #}
cl_kernel a                    = castPtr a
type CL_kernel_info            = {# type cl_kernel_info #}
cl_kernel_info a               = cl_uint a
type CL_kernel_work_group_info = {# type cl_kernel_work_group_info #}
cl_kernel_work_group_info a    = cl_uint a

type CL_profiling_info = {# type cl_profiling_info #}
cl_profiling_info a    = cl_uint a

data CL_image_format =
  CL_image_format { image_channel_order     :: CL_channel_order,
                    image_channel_data_type :: CL_channel_type
                  }
{# pointer *cl_image_format -> CL_image_format nocode #}

getImageChannelOrder s = {#get cl_image_format->image_channel_order#} s
                           >>= return . cl_channel_order

setImageChannelOrder s d = {#set cl_image_format->image_channel_order#}
                             s
                             (cl_channel_order d)

getImageChannelDataType s = {#get cl_image_format->image_channel_data_type#} s
                              >>= return . cl_channel_type

setImageChannelDataType s d = {#set cl_image_format->image_channel_data_type#}
                                s
                                (cl_channel_type d)