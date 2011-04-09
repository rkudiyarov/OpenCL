--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.V10.Program
       ( clCreateProgramWithSource
       , clCreateProgramWithBinary
       , clRetainProgram
       , clReleaseProgram
       , clBuildProgram
       , clUnloadCompiler
       , clGetProgramReferenceCount
       , clGetProgramContext
       , clGetProgramNumDevices
       , clGetProgramDevices
       , clGetProgramSource
       , clGetProgramBinarySizes
       , clGetProgramBinaries
       , clGetProgramBinariesWithSizes
       , clGetProgramBuildStatus
       , clGetProgramBuildOptions
       , clGetProgramBuildLog
       )
       where

import qualified Foreign.OpenCL.Raw.V10 as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.OpenCL.V10.Error
import Foreign.OpenCL.V10.Utils

clCreateProgramWithSource :: Raw.CL_context -> [String] -> IO Raw.CL_program
clCreateProgramWithSource c ss =
    do
    let count = length ss
    let lengths = map (Raw.cl_size . length) ss
    strings <- mapM (newCString) ss
    withArray strings $ \strs ->
     withArray lengths $ \lngs ->
      alloca $ \p_e ->
          do
          pr <- Raw.clCreateProgramWithSource c (Raw.cl_uint count) strs lngs p_e
          mapM (free) strings
          retCode <- peek p_e
          clCheckError retCode $ return pr

clCreateProgramWithBinary :: Raw.CL_context -> [Raw.CL_device_id] -> [(Raw.CL_size, Ptr CUChar)] -> IO (Raw.CL_program, [Raw.CL_bool])
clCreateProgramWithBinary c dvs bns =
    do
    let count = length bns
    let sizes = map (Raw.cl_size . fst) bns
    let ptrs = map (snd) bns
    withArray sizes $ \p_sizes ->
     withArray ptrs $ \p_ptrs ->
      withArray dvs $ \p_dvs ->
       allocaArray count $ \p_bin_status ->
        alloca $ \p_e ->
            do
            pr <- Raw.clCreateProgramWithBinary c (Raw.cl_uint $ length dvs) p_dvs p_sizes p_ptrs p_bin_status p_e
            retCode <- peek p_e
            bin_status <- peekArray count p_bin_status
            clCheckError retCode $ return (pr, map cToEnum bin_status)

clRetainProgram :: Raw.CL_program -> IO ()
clRetainProgram = clRetain Raw.clRetainProgram

clReleaseProgram :: Raw.CL_program -> IO ()
clReleaseProgram = clRelease Raw.clReleaseProgram

--
-- TODO: to implement callback for clBuildProgram
--
clBuildProgram :: Raw.CL_program -> [Raw.CL_device_id] -> String -> IO ()
clBuildProgram p dvs opt =
    withArray dvs $ \p_dvs ->
        withCString opt $ \p_opt ->
            do
            retCode <- Raw.clBuildProgram p (Raw.cl_uint $ length dvs) p_dvs p_opt nullFunPtr nullPtr
            clCheckError retCode $ return ()

clUnloadCompiler :: IO ()
clUnloadCompiler =
    do
    retCode <- Raw.clUnloadCompiler
    clCheckError retCode $ return ()

clGetProgramReferenceCount :: (Integral i) => Raw.CL_program -> IO i
clGetProgramReferenceCount = clGetInfoIntegral Raw.clGetProgramInfo Raw.CLProgramReferenceCount

clGetProgramContext :: Raw.CL_program -> IO Raw.CL_context
clGetProgramContext = clGetInfoObject Raw.clGetProgramInfo Raw.CLProgramContext

clGetProgramNumDevices :: (Integral i) => Raw.CL_program -> IO i
clGetProgramNumDevices = clGetInfoIntegral Raw.clGetProgramInfo Raw.CLProgramNumDevices

clGetProgramDevices :: Raw.CL_program -> IO [Raw.CL_device_id]
clGetProgramDevices = clGetInfoObjectsArray Raw.clGetProgramInfo Raw.CLProgramDevices

clGetProgramSource :: Raw.CL_program -> IO String
clGetProgramSource = clGetInfoString Raw.clGetProgramInfo Raw.CLProgramSource

clGetProgramBinarySizes :: Raw.CL_program -> IO [Raw.CL_size]
clGetProgramBinarySizes = clGetInfoObjectsArray Raw.clGetProgramInfo Raw.CLProgramBinarySizes

clGetProgramBinaries :: Raw.CL_program -> IO [Ptr CUChar]
clGetProgramBinaries = clGetInfoObjectsArray Raw.clGetProgramInfo Raw.CLProgramBinaries

clGetProgramBinariesWithSizes :: Raw.CL_program -> IO [(Raw.CL_size, Ptr CUChar)]
clGetProgramBinariesWithSizes p =
    do
    bin_sizes <- clGetProgramBinarySizes p
    bins <- clGetProgramBinaries p
    return $ zipWith (\x -> \y -> (x,y)) bin_sizes bins

clGetProgramBuildStatus :: Raw.CL_program -> Raw.CL_device_id -> IO Raw.CLBuildStatus
clGetProgramBuildStatus p d =
    do
    buf <- clGetInfoLengthWDI Raw.clGetProgramBuildInfo Raw.CLProgramBuildStatus p d
    allocaBytes buf $ \p_void ->
        do
        retCode <- Raw.clGetProgramBuildInfo p d (cFromEnum Raw.CLProgramBuildStatus) (Raw.cl_uint buf) (p_void :: Ptr CLLong) nullPtr
        clCheckError retCode $ peekEnum p_void

clGetProgramBuildOptions :: Raw.CL_program -> Raw.CL_device_id -> IO String
clGetProgramBuildOptions = clGetInfoStringWDI Raw.clGetProgramBuildInfo Raw.CLProgramBuildOptions

clGetProgramBuildLog :: Raw.CL_program -> Raw.CL_device_id -> IO String
clGetProgramBuildLog = clGetInfoStringWDI Raw.clGetProgramBuildInfo Raw.CLProgramBuildLog
