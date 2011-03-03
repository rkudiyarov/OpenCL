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

clGetPlatformCount :: IO Int
clGetPlatformCount =
    C.alloca $ \p_num ->
        do
        retCode <- Raw.clGetPlatformIDs 0 C.nullPtr p_num
        clCheckError retCode $ C.peekIntConv p_num

clGetPlatformIDs :: IO [Raw.CL_platform_id]
clGetPlatformIDs =
    do
    platform_count <- clGetPlatformCount
    C.allocaArray (Raw.cl_uint platform_count) $ \p_ids ->
        do
        retCode <- Raw.clGetPlatformIDs (Raw.cl_uint platform_count) p_ids C.nullPtr
        clCheckError retCode $ C.peekArray (Raw.cl_uint platform_count) p_ids

clGetPlatformInfo :: Raw.CLPlatformInfo -> Raw.CL_platform_id -> IO String
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
