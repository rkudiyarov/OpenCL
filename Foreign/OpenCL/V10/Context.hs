--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.V10.Context
       ( ContextProperties (..)
       , clCreateContext
       , clCreateContextFromType
       , clRetainContext
       , clReleaseContext
       , clGetContextReferenceCount
       , clGetContextDevices
       )
       where

import qualified Foreign.OpenCL.Raw.V10 as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.Storable
import Foreign.OpenCL.V10.Error
import Foreign.OpenCL.V10.Utils

data ContextProperties = ContextPlatform Raw.CL_platform_id
                       | ContextLastProperty

instance Storable ContextProperties where
    sizeOf (ContextPlatform p) = sizeOf (0 :: CLLong) + sizeOf (p :: Raw.CL_platform_id)
    sizeOf ContextLastProperty = sizeOf (0 :: CLLong)

    alignment _ = alignment (0 :: CLLong)

    peek ptr =
        do
        t <- peekIntConv (castPtr ptr :: Ptr CLLong)
        v <- peekByteOff ptr $ sizeOf (0 :: CLLong)
        let iov | t == 0 = ContextLastProperty
                | t == cFromEnum Raw.CLContextPlatform = ContextPlatform v
                | otherwise = ContextLastProperty
        return iov

    poke ptr (ContextPlatform p) =
        do
        putStrLn "CP 1"
        poke (castPtr ptr :: Ptr CLLong) (cFromEnum Raw.CLContextPlatform)
        putStrLn "CP 2"
        pokeByteOff (castPtr ptr :: Ptr Raw.CL_platform_id) (sizeOf (0 :: CLLong)) p

    poke ptr ContextLastProperty =
        do
        putStrLn "CLP 1"
        poke (castPtr ptr :: Ptr CLLong) 0

-- TODO: to add a callback function with user data to clCreateContext and clCreateContextFromType.
-- TODO: to create data from list of properties, may be through instance of Storable

clCreateContext :: [ContextProperties] -> [Raw.CL_device_id] -> IO Raw.CL_context
clCreateContext ps ds =
    alloca $ \p_err ->
        do
        prop_ptr <- newArray0 ContextLastProperty ps
        dev_ptr <- newArray ds
        context <- Raw.clCreateContext (castPtr prop_ptr) (Raw.cl_uint $ length ds) dev_ptr nullFunPtr nullPtr p_err
        free dev_ptr
        free prop_ptr
        retCode <- peek p_err
        clCheckError retCode $ return context

clCreateContextFromType :: [ContextProperties] -> [Raw.CLDeviceType] -> IO Raw.CL_context
clCreateContextFromType ps dt =
    alloca $ \p_err ->
        do
        prop_ptr <- newArray0 ContextLastProperty ps
        context <- Raw.clCreateContextFromType (castPtr prop_ptr) (combineBitMasks dt) nullFunPtr nullPtr p_err
        free prop_ptr
        retCode <- peek p_err
        clCheckError retCode $ return context

clRetainContext :: Raw.CL_context -> IO ()
clRetainContext = clRetain Raw.clRetainContext

clReleaseContext :: Raw.CL_context -> IO ()
clReleaseContext = clRelease Raw.clReleaseContext

clGetContextReferenceCount :: (Integral i) => Raw.CL_context -> IO i
clGetContextReferenceCount = clGetInfoIntegral Raw.clGetContextInfo Raw.CLContextReferenceCount

clGetContextDevices :: Raw.CL_context -> IO [Raw.CL_device_id]
clGetContextDevices = clGetInfoObjectsArray Raw.clGetContextInfo Raw.CLContextDevices