--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.V10.Memory
       ( clCreateBufferFromPtr
       , clCreateBufferFromList
       , clEnqueueReadBufferToPtr
       , clEnqueueReadBufferToList
       , clEnqueueWriteBufferFromPtr
       , clEnqueueWriteBufferFromList
       , clEnqueueCopyBuffer
       , clRetainMemObject
       , clReleaseMemObject
{-     , clCreateImage2DFromPtr
       , clCreateImage3D
       , clGetSupportedImageFormats
       , clEnqueueReadImage
       , clEnqueueWriteImage
       , clEnqueueCopyImage
       , clEnqueueCopyImageToBuffer
       , clEnqueueCopyBufferToImage
       , clEnqueueMapBuffer
       , clEnqueueMapImage
       , clEnqueueUnmapMemObject
-}     , clGetMemObjectType
       , clGetMemObjectFlags
       , clGetMemObjectInfoIntegral
       , clGetMemObjectSize
       , clGetMemObjectHostPtr
       , clGetMemObjectList
       , clGetMemObjectMapCount
       , clGetMemObjectReferenceCount
       , clGetMemObjectContext
--     , clGetImageFormat
       , clGetImageElementSize
       , clGetImageRowPitch
       , clGetImageSlicePitch
       , clGetImageWidth
       , clGetImageHeight
       , clGetImageDepth
       )
       where

import qualified Foreign.OpenCL.Raw.V10 as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.OpenCL.V10.Error
import Foreign.OpenCL.V10.Utils

clCreateBufferFromPtr :: (Integral i, Storable a) => Raw.CL_context -> [Raw.CLMemFlags] -> i -> Ptr a -> IO Raw.CL_mem
clCreateBufferFromPtr c mf size host_ptr =
    alloca $ \p_err ->
        do
        m <- Raw.clCreateBuffer c (combineBitMasks mf) (Raw.cl_size size) host_ptr p_err
        retCode <- peek p_err
        clCheckError retCode $ return m

--
-- NOTE: It is not posible to use CLMemUseHostPtr
-- in the clCreateBufferFromList function,
-- because data in ptr will be destroyed
-- after the function ends.
--
clCreateBufferFromList :: (Storable a) => Raw.CL_context -> [Raw.CLMemFlags] -> [a] -> IO Raw.CL_mem
clCreateBufferFromList c mf l =
    do
    let size = length l * (sizeOf $ head l)
    host_ptr <- newArray l
    m <- clCreateBufferFromPtr c mf size host_ptr
    free host_ptr
    return m

--
-- offset and cb in bytes
--
clEnqueueReadBufferToPtr :: (Integral i, Storable a) => Raw.CL_command_queue -> Raw.CL_mem -> Raw.CLBool -> i -> i -> Ptr a -> [Raw.CL_event] -> IO Raw.CL_event
clEnqueueReadBufferToPtr cq m b offset cb ptr evs =
    alloca $ \p_ev ->
        do
        let n_evs = Raw.cl_uint $ length evs
        p_evs <- case evs of 
                 [] -> return(nullPtr)
                 otherwise -> newArray evs
        retCode <- Raw.clEnqueueReadBuffer cq m (cFromEnum b) (Raw.cl_size offset) (Raw.cl_size cb) ptr n_evs p_evs p_ev
        free p_evs
        clCheckError retCode $ peek p_ev

--
-- offset and cb in elements
--
-- NOTE: It is not posible to use non-blocking method
-- in the clEnqueueReadBufferToList function,
-- because in this case data in ptr will be destroyed
-- before the function ends.
--
clEnqueueReadBufferToList :: (Integral i, Storable a) => Raw.CL_command_queue -> Raw.CL_mem -> i -> i -> [Raw.CL_event] -> IO ([a], Raw.CL_event)
clEnqueueReadBufferToList cq m offset cb evs =
    allocaArray (fromIntegral cb) $ \ptr ->
        do
        undef <- peek ptr
        let size = fromIntegral $ sizeOf undef
        ev <- clEnqueueReadBufferToPtr cq m Raw.CLTrue (offset * size) (cb * size) ptr evs
        dt <- peekArray (fromIntegral cb) ptr
        return (dt, ev)

--
-- offset and cb in bytes
--
clEnqueueWriteBufferFromPtr :: (Integral i, Storable a) => Raw.CL_command_queue -> Raw.CL_mem -> Raw.CLBool -> i -> i -> Ptr a -> [Raw.CL_event] -> IO Raw.CL_event
clEnqueueWriteBufferFromPtr cq m b offset cb ptr evs =
    alloca $ \p_ev ->
        do
        let n_evs = Raw.cl_uint $ length evs
        p_evs <- case evs of 
                 [] -> return(nullPtr)
                 otherwise -> newArray evs
        retCode <- Raw.clEnqueueWriteBuffer cq m (cFromEnum b) (Raw.cl_size offset) (Raw.cl_size cb) ptr n_evs p_evs p_ev
        free p_evs
        clCheckError retCode $ peek p_ev

--
-- offset in elements
--
-- NOTE: It is not posible to use non-blocking method
-- in the clEnqueueWriteBufferFromList function,
-- because in this case data in ptr will be destroyed
-- before the function ends.
--
clEnqueueWriteBufferFromList :: (Integral i, Storable a) => Raw.CL_command_queue -> Raw.CL_mem -> i -> [a] -> [Raw.CL_event] -> IO Raw.CL_event
clEnqueueWriteBufferFromList cq m offset dt evs =
    do
    let size = fromIntegral $ sizeOf $ head dt
    ptr <- newArray dt
    ev <- clEnqueueWriteBufferFromPtr cq m Raw.CLTrue (offset * size) (size * (fromIntegral $ length dt)) ptr evs
    free ptr
    return ev

clEnqueueCopyBuffer :: (Integral i) => Raw.CL_command_queue -> Raw.CL_mem -> Raw.CL_mem -> i -> i -> i -> [Raw.CL_event] -> IO Raw.CL_event
clEnqueueCopyBuffer cq sm dm soff doff cb evs =
    alloca $ \p_ev ->
        do
        let n_evs = Raw.cl_uint $ length evs
        p_evs <- newArray evs
        retCode <- Raw.clEnqueueCopyBuffer cq sm dm (Raw.cl_size soff) (Raw.cl_size doff) (Raw.cl_size cb) n_evs p_evs p_ev
        free p_evs
        clCheckError retCode $ peek p_ev

clRetainMemObject :: Raw.CL_mem -> IO ()
clRetainMemObject = clRetain Raw.clRetainMemObject

clReleaseMemObject :: Raw.CL_mem -> IO ()
clReleaseMemObject = clRelease Raw.clReleaseMemObject

{-
clCreateImage2DFromPtr :: (Integral i, Storable a) => Raw.CL_context -> [Raw.CLMemFlags] -> Raw.CL_image_format -> i -> i -> i -> Ptr a -> IO Raw.CL_mem
clCreateImage2DFromPtr c mf image_format image_width image_height image_row_pitch host_ptr =
    alloca $ \p_err ->
        do
        m <- Raw.clCreateImage2D c (combineBitMasks mf) image_format (Raw.cl_size image_width) (Raw.cl_size image_height) (Raw.cl_size image_row_pitch) host_ptr p_err
        retCode <- peek p_err
        clCheckError retCode $ return m

       , clCreateImage3D
       , clGetSupportedImageFormats
       , clEnqueueReadImage
       , clEnqueueWriteImage
       , clEnqueueCopyImage
       , clEnqueueCopyImageToBuffer
       , clEnqueueCopyBufferToImage
       , clEnqueueMapBuffer
       , clEnqueueMapImage
       , clEnqueueUnmapMemObject
-}

clGetMemObjectType :: Raw.CL_mem -> IO Raw.CLMemObjectType
clGetMemObjectType = clGetInfoEnum Raw.clGetMemObjectInfo Raw.CLMemType

clGetMemObjectFlags :: Raw.CL_mem -> IO [Raw.CLMemFlags]
clGetMemObjectFlags = clGetInfoBitfield Raw.clGetMemObjectInfo Raw.CLMemFlags

clGetMemObjectInfoIntegral :: (Integral i) => Raw.CLMemInfo -> Raw.CL_mem -> IO i
clGetMemObjectInfoIntegral = clGetInfoIntegral Raw.clGetMemObjectInfo

clGetMemObjectSize :: (Integral i) => Raw.CL_mem -> IO i
clGetMemObjectSize = clGetMemObjectInfoIntegral Raw.CLMemSize

clGetMemObjectHostPtr :: (Storable a) => Raw.CL_mem -> IO (Ptr a)
clGetMemObjectHostPtr = clGetInfoObject Raw.clGetMemObjectInfo Raw.CLMemHostPtr

clGetMemObjectList :: (Storable a) => Raw.CL_mem -> IO [a]
clGetMemObjectList m =
    do
    size <- clGetMemObjectSize m
    ptr <- clGetMemObjectHostPtr m
    obj <- peek ptr
    let s = size `div` sizeOf obj
    peekArray s ptr

clGetMemObjectMapCount :: (Integral i) => Raw.CL_mem -> IO i
clGetMemObjectMapCount = clGetMemObjectInfoIntegral Raw.CLMemMapCount

clGetMemObjectReferenceCount :: (Integral i) => Raw.CL_mem -> IO i
clGetMemObjectReferenceCount = clGetMemObjectInfoIntegral Raw.CLMemReferenceCount

clGetMemObjectContext :: Raw.CL_mem -> IO Raw.CL_context
clGetMemObjectContext = clGetInfoObject Raw.clGetMemObjectInfo Raw.CLMemContext

--clGetImageFormat :: Raw.CL_mem -> IO

clGetImageInfoIntegral :: (Integral i) => Raw.CLImageInfo -> Raw.CL_mem -> IO i
clGetImageInfoIntegral = clGetInfoIntegral Raw.clGetImageInfo

clGetImageElementSize :: (Integral i) => Raw.CL_mem -> IO i
clGetImageElementSize = clGetImageInfoIntegral Raw.CLImageElementSize

clGetImageRowPitch :: (Integral i) => Raw.CL_mem -> IO i
clGetImageRowPitch = clGetImageInfoIntegral Raw.CLImageRowPitch

clGetImageSlicePitch :: (Integral i) => Raw.CL_mem -> IO i
clGetImageSlicePitch = clGetImageInfoIntegral Raw.CLImageSlicePitch

clGetImageWidth :: (Integral i) => Raw.CL_mem -> IO i
clGetImageWidth = clGetImageInfoIntegral Raw.CLImageWidth

clGetImageHeight :: (Integral i) => Raw.CL_mem -> IO i
clGetImageHeight = clGetImageInfoIntegral Raw.CLImageHeight

clGetImageDepth :: (Integral i) => Raw.CL_mem -> IO i
clGetImageDepth = clGetImageInfoIntegral Raw.CLImageDepth
