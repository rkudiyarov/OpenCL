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
       , clGetContextProperties
       )
       where

import qualified Foreign.OpenCL.Raw.V10 as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.Storable
import Foreign.OpenCL.V10.Error
import Foreign.OpenCL.V10.Utils

data ContextProperties = ContextPlatform Raw.CL_platform_id
                       | ContextLastProperty
                       deriving (Show, Eq)

instance Storable ContextProperties where
    sizeOf (ContextPlatform p) = sizeOf (undefined :: Raw.CL_context_properties) + sizeOf (undefined :: Raw.CL_platform_id)
    sizeOf ContextLastProperty = sizeOf (undefined :: Raw.CL_context_properties)

    alignment _ = alignment (undefined :: Raw.CL_context_properties)

    peek ptr =
        do
        t <- peekIntConv (castPtr ptr :: Ptr Raw.CL_context_properties)
        v <- peekByteOff ptr $ sizeOf (undefined :: Raw.CL_context_properties)
        let iov | t == 0 = ContextLastProperty
                | t == cFromEnum Raw.CLContextPlatform = ContextPlatform v
                | otherwise = ContextLastProperty
        return iov

    poke ptr (ContextPlatform p) =
        do
        poke (castPtr ptr :: Ptr Raw.CL_context_properties) (cFromEnum Raw.CLContextPlatform)
        pokeByteOff (castPtr ptr :: Ptr Raw.CL_platform_id) (sizeOf (undefined :: Ptr Raw.CL_context_properties)) p

    poke ptr ContextLastProperty =
        do
        poke (castPtr ptr :: Ptr Raw.CL_context_properties) 0

propListToData :: [ContextProperties] -> IO (Ptr ContextProperties)
propListToData [] = return( nullPtr )
propListToData ps =
    do
    ptr <- mallocBytes $ sizeOfList ps 0
    writeData ptr ps
    return ptr
    where sizeOfList [] s = s
          sizeOfList (l:ls) s = sizeOfList ls (s + sizeOf l)
          writeData ptr [] = poke ptr ContextLastProperty >> return ()
          writeData ptr (p:ps) = do
                                 poke ptr p
                                 writeData (ptr `plusPtr` sizeOf p) ps

propDataToList :: Ptr ContextProperties -> IO [ContextProperties]
propDataToList ptr =
    do
    readData ptr []
    where readData p ps =
              do
              prop <- peek p
              case prop of
                  ContextLastProperty -> return ps
                  (ContextPlatform pl) -> readData (p `plusPtr` sizeOf prop) (prop:ps)

-- TODO: to add a callback function with user data to clCreateContext and clCreateContextFromType.
-- TODO: to create data from list of properties, may be through instance of Storable

clCreateContext :: [ContextProperties] -> [Raw.CL_device_id] -> IO Raw.CL_context
clCreateContext ps ds =
    alloca $ \p_err -> 
        do
        prop_ptr <- propListToData ps
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
        prop_ptr <- propListToData ps
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

clGetContextProperties :: Raw.CL_context -> IO [ContextProperties]
clGetContextProperties c =
    do
    buf <- clGetInfoLength Raw.clGetContextInfo Raw.CLContextProperties c
    allocaBytes buf $ \p_void ->
        do
        retCode <- Raw.clGetContextInfo c (cFromEnum Raw.CLContextProperties) (Raw.cl_uint buf) p_void nullPtr
        ob <- peek p_void
        clCheckError retCode $ propDataToList p_void
