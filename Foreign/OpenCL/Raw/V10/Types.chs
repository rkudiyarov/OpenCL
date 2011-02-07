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

type CL_size     = {# type size_t #}
cl_size a        = cIntConv a
type CL_char     = {# type cl_char #}
cl_char a        = cIntConv a
type CL_uchar    = {# type cl_uchar #}
cl_uchar a       = cIntConv a
type CL_short    = {# type cl_short #}
cl_short a     	 = cIntConv a
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

type CL_device_id   = {# type cl_device_id #}
cl_device_id a      = castPtr a
type CL_device_type = {# type cl_device_type #}
cl_device_type a    = cl_bitfield a
type CL_device_info = {# type cl_device_info #}
cl_device_info a    = cl_uint a

type CL_context            = {# type cl_context #}
cl_context a               = castPtr a
type CL_context_properties = {# type cl_context_properties #}
cl_context_properties a    = castPtr a
type CL_context_info       = {# type cl_context_info #}
cl_context_info a          = cl_uint a
