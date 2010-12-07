{-# LANGUAGE ForeignFunctionInterface #-}

module Foreign.OpenCL.Raw.V10.Types where

#include "../inc_opencl.h"

import Foreign
import Foreign.C
import Foreign.OpenCL.Raw.C2HS

type CLPlatformID = {# type cl_platform_id #}

type CLDeviceID = {# type cl_device_id #}