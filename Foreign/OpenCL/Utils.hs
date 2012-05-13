--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2012] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.Utils
       ( simpleFunction
       , clRetain
       , clRelease
       , clGetInfoLength
       , clGetInfoString
       , clGetInfoIntegral
       , clGetInfoEnum
       , clGetInfoBitfield
       , clGetInfoObject
       , clGetInfoObjectsArray
       , clGetInfoLengthWDI
       , clGetInfoIntegralWDI
       , clGetInfoStringWDI
       , withArrayOrNP
       )
       where

import qualified Foreign.OpenCL.Raw as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.OpenCL.Error

simpleFunction :: (a -> IO Raw.CL_int) -> a -> IO ()
simpleFunction f o =
    do
    retCode <- f o
    clCheckError retCode $ return ()

--
-- Retain and release functions the same for all objects.
--

clRetain :: (a -> IO Raw.CL_int) -> a -> IO ()
clRetain = simpleFunction

clRelease :: (a -> IO Raw.CL_int) -> a -> IO ()
clRelease = simpleFunction

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

--
-- Functions for getting information from opencl functions,
-- which takes object and device_id.
--

type FunInfoWDI a e b = (a -> Raw.CL_device_id -> e -> Raw.CL_size -> Ptr b -> Ptr Raw.CL_size -> IO Raw.CL_int)

clGetInfoLengthWDI :: (Integral t, Enum e, Integral i) => FunInfoWDI a t b -> e -> a -> Raw.CL_device_id -> IO i
clGetInfoLengthWDI f i o d =
    alloca $ \p_size ->
        do
        rC <- f o d (cFromEnum i) 0 nullPtr p_size
        clCheckError rC $ peekIntConv p_size

clGetInfoIntegralWDI :: (Integral t, Enum e, Integral i) => FunInfoWDI a t CULLong -> e -> a -> Raw.CL_device_id -> IO i
clGetInfoIntegralWDI f i o d =
    do
    buf <- clGetInfoLengthWDI f i o d
    allocaBytes buf $ \p_void ->
        do
        retCode <- f o d (cFromEnum i) (Raw.cl_uint buf) p_void nullPtr
        clCheckError retCode $ peekIntConv p_void

clGetInfoStringWDI :: (Integral t, Enum e) => FunInfoWDI a t CChar -> e -> a -> Raw.CL_device_id -> IO String
clGetInfoStringWDI f i o d =
    do
    buf <- clGetInfoLengthWDI f i o d
    allocaBytes buf $ \p_void ->
        do
        retCode <- f o d (cFromEnum i) (Raw.cl_uint buf) p_void nullPtr
        clCheckError retCode $ peekCString p_void

-- |The same as withArray but return nullPtr for empty lists
withArrayOrNP :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withArrayOrNP [] f = f nullPtr
withArrayOrNP l f  = withArray l f
