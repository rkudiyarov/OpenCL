--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Context
       ( clCreateContext
       , clCreateContextFromType
       , clRetainContext
       , clReleaseContext
       , clGetContextInfo
       )
       where

#include <inc_opencl.h>

import Foreign.OpenCL.Raw.C2HS
import Foreign.OpenCL.Raw.V10.Types

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
