--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.HighLevel.V10.Platform
       ( clGetPlatformCount
       , clGetPlatformIDs
       , clGetPlatformInfo
       , clGetPlatformProfile
       , clGetPlatformVersion
       , clGetPlatformName
       , clGetPlatformVendor
       , clGetPlatformExtensions
       )
       where

import qualified Foreign.OpenCL.Raw.V10 as Raw
import qualified Foreign.OpenCL.Raw.C2HS as C

import Foreign.OpenCL.HighLevel.V10.Error

clGetPlatformCount =
    C.alloca $ \p_num ->
        do
        retCode <- Raw.clGetPlatformIDs 0 C.nullPtr p_num
        clCheckError retCode $ C.peekIntConv p_num

clGetPlatformIDs =
    do
    platform_count <- clGetPlatformCount
    C.allocaArray (Raw.cl_uint platform_count) $ \p_ids ->
        do
        retCode <- Raw.clGetPlatformIDs platform_count p_ids C.nullPtr
        clCheckError retCode $ C.peekArray (Raw.cl_uint platform_count) p_ids

clGetPlatformInfo info platform =
    do
    buf <- clGetInfoLength info platform
    C.allocaBytes buf $ \p_void ->
        do
        retCode <- Raw.clGetPlatformInfo platform (C.cFromEnum info) (Raw.cl_uint buf) (p_void) C.nullPtr
        clCheckError retCode $ C.peekCString p_void
    where clGetInfoLength i p =
              C.alloca $ \p_size ->
                  do
                  rC <- Raw.clGetPlatformInfo p (C.cFromEnum i) 0 C.nullPtr p_size
                  clCheckError rC $ C.peekIntConv p_size

clGetPlatformProfile = clGetPlatformInfo Raw.CLPlatformProfile
clGetPlatformVersion = clGetPlatformInfo Raw.CLPlatformVersion
clGetPlatformName = clGetPlatformInfo Raw.CLPlatformName
clGetPlatformVendor = clGetPlatformInfo Raw.CLPlatformVendor
clGetPlatformExtensions = clGetPlatformInfo Raw.CLPlatformExtensions
