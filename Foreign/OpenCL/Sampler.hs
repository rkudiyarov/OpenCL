--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2012] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.Sampler
       ( clCreateSampler
       , clRetainSampler
       , clReleaseSampler
       , clGetSamplerReferenceCount
       , clGetSamplerContext
       , clGetSamplerAddressingMode
       , clGetSamplerFilterMode
       , clGetSamplerNormalizedCoords
       )
       where

import qualified Foreign.OpenCL.Raw as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.OpenCL.Error
import Foreign.OpenCL.Utils

clCreateSampler :: Raw.CL_context -> Raw.CLBool -> Raw.CLAddressingMode -> Raw.CLFilterMode -> IO Raw.CL_sampler
clCreateSampler c b am fm =
    alloca $ \p_err ->
        do
        s <- Raw.clCreateSampler c (cFromEnum b) (cFromEnum am) (cFromEnum fm) p_err
        retCode <- peek p_err
        clCheckError retCode $ return s

clRetainSampler :: Raw.CL_sampler -> IO ()
clRetainSampler = clRetain Raw.clRetainSampler

clReleaseSampler :: Raw.CL_sampler -> IO ()
clReleaseSampler = clRelease Raw.clReleaseSampler

clGetSamplerReferenceCount :: (Integral i) => Raw.CL_sampler -> IO i
clGetSamplerReferenceCount = clGetInfoIntegral Raw.clGetSamplerInfo Raw.CLSamplerReferenceCount

clGetSamplerContext :: Raw.CL_sampler -> IO Raw.CL_context
clGetSamplerContext = clGetInfoObject Raw.clGetSamplerInfo Raw.CLSamplerContext

clGetSamplerInfoEnum :: (Enum e) => Raw.CLSamplerInfo -> Raw.CL_sampler -> IO e
clGetSamplerInfoEnum = clGetInfoEnum Raw.clGetSamplerInfo

clGetSamplerAddressingMode :: Raw.CL_sampler -> IO Raw.CLAddressingMode
clGetSamplerAddressingMode = clGetSamplerInfoEnum Raw.CLSamplerAddressingMode

clGetSamplerFilterMode :: Raw.CL_sampler -> IO Raw.CLFilterMode
clGetSamplerFilterMode = clGetSamplerInfoEnum Raw.CLSamplerFilterMode

clGetSamplerNormalizedCoords :: Raw.CL_sampler -> IO Raw.CLBool
clGetSamplerNormalizedCoords = clGetSamplerInfoEnum Raw.CLSamplerNormalizedCoords
