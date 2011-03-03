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
import Foreign.OpenCL.Raw.C2HS

import Foreign.OpenCL.HighLevel.V10.Error

clGetPlatformCount :: IO Int
clGetPlatformCount =
    alloca $ \p_num ->
        do
        retCode <- Raw.clGetPlatformIDs 0 nullPtr p_num
        clCheckError retCode $ peekIntConv p_num

clGetPlatformIDs :: IO [Raw.CL_platform_id]
clGetPlatformIDs =
    do
    platform_count <- clGetPlatformCount
    allocaArray (Raw.cl_uint platform_count) $ \p_ids ->
        do
        retCode <- Raw.clGetPlatformIDs (Raw.cl_uint platform_count) p_ids nullPtr
        clCheckError retCode $ peekArray (Raw.cl_uint platform_count) p_ids

clGetPlatformInfo :: Raw.CLPlatformInfo -> Raw.CL_platform_id -> IO String
clGetPlatformInfo info platform =
    do
    buf <- clGetInfoLength info platform
    allocaBytes buf $ \p_void ->
        do
        retCode <- Raw.clGetPlatformInfo platform (cFromEnum info) (Raw.cl_uint buf) p_void nullPtr
        clCheckError retCode $ peekCString p_void
    where clGetInfoLength i p =
              alloca $ \p_size ->
                  do
                  rC <- Raw.clGetPlatformInfo p (cFromEnum i) 0 nullPtr p_size
                  clCheckError rC $ peekIntConv p_size

clGetPlatformProfile :: Raw.CL_platform_id -> IO String
clGetPlatformProfile = clGetPlatformInfo Raw.CLPlatformProfile

clGetPlatformVersion :: Raw.CL_platform_id -> IO String
clGetPlatformVersion = clGetPlatformInfo Raw.CLPlatformVersion

clGetPlatformName :: Raw.CL_platform_id -> IO String
clGetPlatformName = clGetPlatformInfo Raw.CLPlatformName

clGetPlatformVendor :: Raw.CL_platform_id -> IO String
clGetPlatformVendor = clGetPlatformInfo Raw.CLPlatformVendor

clGetPlatformExtensions :: Raw.CL_platform_id -> IO String
clGetPlatformExtensions = clGetPlatformInfo Raw.CLPlatformExtensions
