--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.HighLevel.V10.Utils
       ( clRetain
       , clRelease
       , clGetInfoLength
       , clGetInfoString
       , clGetInfoIntegral
       , clGetInfoEnum
       , clGetInfoBitfield
       , clGetInfoObject
       , clGetInfoObjectsArray
       )
       where

import qualified Foreign.OpenCL.Raw.V10 as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.OpenCL.HighLevel.V10.Error

--
-- Retain and release functions the same for all objects.
--

clRetain :: (a -> IO Raw.CL_int) -> a -> IO ()
clRetain f o =
    do
    retCode <- f o
    clCheckError retCode $ return ()

clRelease :: (a -> IO Raw.CL_int) -> a -> IO ()
clRelease f o =
    do
    retCode <- f o
    clCheckError retCode $ return ()

--
-- Functions for getting information from all objects.
--

type FunInfo a e b = (a -> e -> Raw.CL_size -> Ptr b -> Ptr Raw.CL_size -> IO Raw.CL_int)

clGetInfoLength :: (Integral t, Enum e, Integral i) => FunInfo a t b -> e -> a -> IO i
clGetInfoLength f i o =
    alloca $ \p_size ->
        do
        rC <- f o (cFromEnum i) 0 nullPtr p_size
        clCheckError rC $ peekIntConv p_size

clGetInfoString :: (Integral t, Enum e) => FunInfo a t CChar -> e -> a -> IO String
clGetInfoString f i o =
    do
    buf <- clGetInfoLength f i o
    allocaBytes buf $ \p_void ->
        do
        retCode <- f o (cFromEnum i) (Raw.cl_uint buf) p_void nullPtr
        clCheckError retCode $ peekCString p_void

clGetInfoIntegral :: (Integral t, Enum e, Integral i) => FunInfo a t CULLong -> e -> a -> IO i
clGetInfoIntegral f i o =
    do
    buf <- clGetInfoLength f i o
    allocaBytes buf $ \p_void ->
        do
        retCode <- f o (cFromEnum i) (Raw.cl_uint buf) p_void nullPtr
        clCheckError retCode $ peekIntConv p_void

clGetInfoEnum :: (Integral t, Enum e, Enum ee) => FunInfo a t CLLong -> e -> a -> IO ee
clGetInfoEnum f i o =
    do
    buf <- clGetInfoLength f i o
    allocaBytes buf $ \p_void ->
        do
        retCode <- f o (cFromEnum i) (Raw.cl_uint buf) p_void nullPtr
        clCheckError retCode $ peekEnum p_void

clGetInfoBitfield :: (Integral t, Enum e, Enum ee) => FunInfo a t CLLong -> e -> a -> IO [ee]
clGetInfoBitfield f i o =
    do
    buf <- clGetInfoLength f i o
    allocaBytes buf $ \p_void ->
        do
        retCode <- f o (cFromEnum i) (Raw.cl_uint buf) p_void nullPtr
        bits <- peekIntConv p_void :: IO CLLong
        clCheckError retCode $ return $ extractBitMasks bits

clGetInfoObject :: (Integral t, Storable s, Enum e) => FunInfo a t s -> e -> a -> IO s
clGetInfoObject f i o =
    do
    buf <- clGetInfoLength f i o
    allocaBytes buf $ \p_void ->
        do
        retCode <- f o (cFromEnum i) (Raw.cl_uint buf) p_void nullPtr
        clCheckError retCode $ peek p_void

clGetInfoObjectsArray :: (Integral t, Storable s, Enum e) => FunInfo a t s -> e -> a -> IO [s]
clGetInfoObjectsArray f i o =
    do
    buf <- clGetInfoLength f i o
    allocaBytes buf $ \p_void ->
        do
        retCode <- f o (cFromEnum i) (Raw.cl_uint buf) p_void nullPtr
        ob <- peek p_void
        clCheckError retCode $ peekArray (buf `div` sizeOf ob) p_void
