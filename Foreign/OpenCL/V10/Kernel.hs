--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.V10.Kernel
       ( clCreateKernel
       , clCreateKernelsInProgram
       , clRetainKernel
       , clReleaseKernel
       , clSetKernelArg
       , clGetKernelFunctionName
       , clGetKernelNumArgs
       , clGetKernelReferenceCount
       , clGetKernelContext
       , clGetKernelProgram
       , clGetKernelWorkGroupSize
       , clGetKernelCompileWorkGroupSize
       , clGetKernelLocalMemSize
       , clEnqueueNDRangeKernel
       , clEnqueueTask
--       , clEnqueueNativeKernel
       )
       where

import qualified Foreign.OpenCL.Raw.V10 as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.OpenCL.V10.Error
import Foreign.OpenCL.V10.Utils

clCreateKernel :: Raw.CL_program -> String -> IO Raw.CL_kernel
clCreateKernel p s =
    withCString s $ \str ->
    alloca $ \p_e ->
        do
        k <- Raw.clCreateKernel p str p_e
        retCode <- peek p_e
        clCheckError retCode $ return k

clCreateKernelsInProgram :: Raw.CL_program -> IO [Raw.CL_kernel]
clCreateKernelsInProgram p =
    do
    nums <- clGetNumKernelsInProgram p
    allocaArray nums $ \p_ks ->
        do
        retCode <- Raw.clCreateKernelsInProgram p (Raw.cl_uint nums) (p_ks :: Ptr Raw.CL_kernel) nullPtr
        clCheckError retCode $ peekArray nums p_ks
    where clGetNumKernelsInProgram pr =
              alloca $ \p_n ->
                  do
                  rC <- Raw.clCreateKernelsInProgram p 0 nullPtr p_n
                  clCheckError rC $ peekIntConv p_n

clRetainKernel :: Raw.CL_kernel -> IO ()
clRetainKernel = clRetain Raw.clRetainKernel

clReleaseKernel :: Raw.CL_kernel -> IO ()
clReleaseKernel = clRelease Raw.clReleaseKernel

clSetKernelArg :: (Integral i, Storable a) => Raw.CL_kernel -> i -> a -> IO ()
clSetKernelArg k i a =
    with a $ \p_a ->
        do
        retCode <- Raw.clSetKernelArg k (Raw.cl_uint i) (Raw.cl_size $ sizeOf a) p_a
        clCheckError retCode $ return ()

clGetKernelFunctionName :: Raw.CL_kernel -> IO String
clGetKernelFunctionName = clGetInfoString Raw.clGetKernelInfo Raw.CLKernelFunctionName

clGetKernelNumArgs :: (Integral i) => Raw.CL_kernel -> IO i
clGetKernelNumArgs = clGetInfoIntegral Raw.clGetKernelInfo Raw.CLKernelNumArgs

clGetKernelReferenceCount :: (Integral i) => Raw.CL_kernel -> IO i
clGetKernelReferenceCount = clGetInfoIntegral Raw.clGetKernelInfo Raw.CLKernelReferenceCount

clGetKernelContext :: Raw.CL_kernel -> IO Raw.CL_context
clGetKernelContext = clGetInfoObject Raw.clGetKernelInfo Raw.CLKernelContext

clGetKernelProgram :: Raw.CL_kernel -> IO Raw.CL_context
clGetKernelProgram = clGetInfoObject Raw.clGetKernelInfo Raw.CLKernelProgram

clGetKernelWorkGroupSize :: (Integral i) => Raw.CL_kernel -> Raw.CL_device_id -> IO i
clGetKernelWorkGroupSize = clGetInfoIntegralWDI Raw.clGetKernelWorkGroupInfo Raw.CLKernelWorkGroupSize

clGetKernelCompileWorkGroupSize :: (Integral i) => Raw.CL_kernel -> Raw.CL_device_id -> IO [i]
clGetKernelCompileWorkGroupSize k d =
    do
    buf <- clGetInfoLengthWDI Raw.clGetKernelWorkGroupInfo Raw.CLKernelCompileWorkGroupSize k d
    allocaArray 3 $ \p_gs ->
        do
        retCode <- Raw.clGetKernelWorkGroupInfo k d (cFromEnum Raw.CLKernelCompileWorkGroupSize) (Raw.cl_uint buf) (p_gs :: Ptr Raw.CL_size) nullPtr
        gs <- peekArray 3 p_gs
        clCheckError retCode $ return $ map (fromIntegral) gs

clGetKernelLocalMemSize :: (Integral i) => Raw.CL_kernel -> Raw.CL_device_id -> IO i
clGetKernelLocalMemSize = clGetInfoIntegralWDI Raw.clGetKernelWorkGroupInfo Raw.CLKernelLocalMemSize

--
-- global work offset should be nullPtr in opencl 1.0,
-- but not local work size
-- FIXME: [] => nullPtr
--

clEnqueueNDRangeKernel :: Raw.CL_command_queue -> Raw.CL_kernel -> [Raw.CL_size] -> [Raw.CL_size] -> [Raw.CL_size] -> [Raw.CL_event] -> IO Raw.CL_event
clEnqueueNDRangeKernel cq k gwo gws lws evs =
    withArrayOrNP gwo $ \p_gwo ->
        withArrayOrNP gws $ \p_gws ->
            withArrayOrNP lws $ \p_lws ->
                withArrayOrNP evs $ \p_evs ->
                    alloca $ \p_e ->
                        do
                        let wd = max (length lws) $ max (length gwo) (length gws)
                        retCode <- Raw.clEnqueueNDRangeKernel cq k (Raw.cl_size wd) nullPtr{-p_gwo-} p_gws nullPtr{-p_lws-} (Raw.cl_uint $ length evs) p_evs p_e
                        clCheckError retCode $ peek p_e

clEnqueueTask :: Raw.CL_command_queue -> Raw.CL_kernel -> [Raw.CL_event] -> IO Raw.CL_event
clEnqueueTask cq k evs =
    withArray evs $ \p_evs ->
        alloca $ \p_e ->
            do
            retCode <- Raw.clEnqueueTask cq k (Raw.cl_uint $ length evs) p_evs p_e
            clCheckError retCode $ peek p_e
