--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2012] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.Device
       ( clGetDeviceCount
       , clGetDeviceIDs
       , clGetDeviceType
       , clGetDeviceVendorId
       , clGetDeviceMaxComputeUnits
       , clGetDeviceMaxWorkItemDimensions
       , clGetDeviceMaxWorkItemSizes
       , clGetDeviceMaxWorkGroupSize
       , clGetDevicePreferredVectorWidthChar
       , clGetDevicePreferredVectorWidthShort
       , clGetDevicePreferredVectorWidthInt
       , clGetDevicePreferredVectorWidthLong
       , clGetDevicePreferredVectorWidthFloat
       , clGetDevicePreferredVectorWidthDouble
       , clGetDeviceMaxClockFrequency
       , clGetDeviceAddressBits
       , clGetDeviceMaxMemAllocSize
       , clGetDeviceImageSupport
       , clGetDeviceMaxReadImageArgs
       , clGetDeviceMaxWriteImageArgs
       , clGetDeviceImage2dMaxWidth
       , clGetDeviceImage2dMaxHeight
       , clGetDeviceImage3dMaxWidth
       , clGetDeviceImage3dMaxHeight
       , clGetDeviceImage3dMaxDepth
       , clGetDeviceMaxSamplers
       , clGetDeviceMaxParameterSize
       , clGetDeviceMemBaseAddrAlign
       , clGetDeviceMinDataTypeAlignSize
       , clGetDeviceSingleFpConfig
       , clGetDeviceGlobalMemCacheType
       , clGetDeviceGlobalMemCachelineSize
       , clGetDeviceGlobalMemCacheSize
       , clGetDeviceGlobalMemSize
       , clGetDeviceMaxConstantBufferSize
       , clGetDeviceMaxConstantArgs
       , clGetDeviceLocalMemType
       , clGetDeviceLocalMemSize
       , clGetDeviceErrorCorrectionSupport
       , clGetDeviceProfilingTimerResolution
       , clGetDeviceEndianLittle
       , clGetDeviceAvailable
       , clGetDeviceCompilerAvailable
       , clGetDeviceExecutionCapabilities
       , clGetDeviceQueueProperties
       , clGetDevicePlatform
       , clGetDeviceName
       , clGetDeviceVendor
       , clGetDriverVersion
       , clGetDeviceProfile
       , clGetDeviceVersion
       , clGetDeviceExtensions
       )
       where

import qualified Foreign.OpenCL.Raw as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.OpenCL.Error
import Foreign.OpenCL.Utils

clGetDeviceCount :: Raw.CL_platform_id -> [Raw.CLDeviceType] -> IO Int
clGetDeviceCount platform device_type =
    alloca $ \p_num ->
        do
        retCode <- Raw.clGetDeviceIDs platform (combineBitMasks device_type) 0 nullPtr p_num
        clCheckError retCode $ peekIntConv p_num

clGetDeviceIDs :: Raw.CL_platform_id -> [Raw.CLDeviceType] -> IO [Raw.CL_device_id]
clGetDeviceIDs platform device_type =
    do
    device_count <- clGetDeviceCount platform device_type
    allocaArray (Raw.cl_uint device_count) $ \p_ids ->
        do
        retCode <- Raw.clGetDeviceIDs platform (combineBitMasks device_type) (Raw.cl_uint device_count) p_ids nullPtr
        clCheckError retCode $ peekArray (Raw.cl_uint device_count) p_ids

clGetDeviceInfoString :: Raw.CLDeviceInfo -> Raw.CL_device_id -> IO String
clGetDeviceInfoString = clGetInfoString Raw.clGetDeviceInfo

clGetDeviceInfoIntegral :: (Integral i) => Raw.CLDeviceInfo -> Raw.CL_device_id -> IO i
clGetDeviceInfoIntegral = clGetInfoIntegral Raw.clGetDeviceInfo

clGetDeviceInfoEnum :: (Enum e) => Raw.CLDeviceInfo -> Raw.CL_device_id -> IO e
clGetDeviceInfoEnum = clGetInfoEnum Raw.clGetDeviceInfo

clGetDeviceInfoBitfield :: (Enum e) => Raw.CLDeviceInfo -> Raw.CL_device_id -> IO [e]
clGetDeviceInfoBitfield = clGetInfoBitfield Raw.clGetDeviceInfo

clGetDeviceType :: Raw.CL_device_id -> IO [Raw.CLDeviceType]
clGetDeviceType = clGetDeviceInfoBitfield Raw.CLDeviceType

clGetDeviceVendorId :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceVendorId = clGetDeviceInfoIntegral Raw.CLDeviceVendorId

clGetDeviceMaxComputeUnits :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMaxComputeUnits = clGetDeviceInfoIntegral Raw.CLDeviceMaxComputeUnits

clGetDeviceMaxWorkItemDimensions :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMaxWorkItemDimensions = clGetDeviceInfoIntegral Raw.CLDeviceMaxWorkItemDimensions

clGetDeviceMaxWorkItemSizes :: (Integral i) => Raw.CL_device_id -> IO [i]
clGetDeviceMaxWorkItemSizes device =
    do
    buf <- clGetInfoLength Raw.clGetDeviceInfo Raw.CLDeviceMaxWorkItemSizes device
    dims <- clGetDeviceMaxWorkItemDimensions device
    allocaArray (Raw.cl_uint dims) $ \p_sizes ->
        do
        retCode <- Raw.clGetDeviceInfo device (cFromEnum Raw.CLDeviceMaxWorkItemSizes) (Raw.cl_uint buf) (p_sizes :: Ptr Raw.CL_size) nullPtr
        clCheckError retCode $ peekArray (Raw.cl_uint dims) p_sizes >>= return . map (cIntConv)

clGetDeviceMaxWorkGroupSize :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMaxWorkGroupSize = clGetDeviceInfoIntegral Raw.CLDeviceMaxWorkGroupSize

clGetDevicePreferredVectorWidthChar :: (Integral i) => Raw.CL_device_id -> IO i
clGetDevicePreferredVectorWidthChar = clGetDeviceInfoIntegral Raw.CLDevicePreferredVectorWidthChar

clGetDevicePreferredVectorWidthShort :: (Integral i) => Raw.CL_device_id -> IO i
clGetDevicePreferredVectorWidthShort = clGetDeviceInfoIntegral Raw.CLDevicePreferredVectorWidthShort

clGetDevicePreferredVectorWidthInt :: (Integral i) => Raw.CL_device_id -> IO i
clGetDevicePreferredVectorWidthInt = clGetDeviceInfoIntegral Raw.CLDevicePreferredVectorWidthInt

clGetDevicePreferredVectorWidthLong :: (Integral i) => Raw.CL_device_id -> IO i
clGetDevicePreferredVectorWidthLong = clGetDeviceInfoIntegral Raw.CLDevicePreferredVectorWidthLong

clGetDevicePreferredVectorWidthFloat :: (Integral i) => Raw.CL_device_id -> IO i
clGetDevicePreferredVectorWidthFloat = clGetDeviceInfoIntegral Raw.CLDevicePreferredVectorWidthFloat

clGetDevicePreferredVectorWidthDouble :: (Integral i) => Raw.CL_device_id -> IO i
clGetDevicePreferredVectorWidthDouble = clGetDeviceInfoIntegral Raw.CLDevicePreferredVectorWidthDouble

clGetDeviceMaxClockFrequency :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMaxClockFrequency = clGetDeviceInfoIntegral Raw.CLDeviceMaxClockFrequency

clGetDeviceAddressBits :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceAddressBits = clGetDeviceInfoIntegral Raw.CLDeviceAddressBits

clGetDeviceMaxMemAllocSize :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMaxMemAllocSize = clGetDeviceInfoIntegral Raw.CLDeviceMaxMemAllocSize

clGetDeviceImageSupport :: Raw.CL_device_id -> IO Raw.CLBool
clGetDeviceImageSupport = clGetDeviceInfoEnum Raw.CLDeviceImageSupport

clGetDeviceMaxReadImageArgs :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMaxReadImageArgs = clGetDeviceInfoIntegral Raw.CLDeviceMaxReadImageArgs

clGetDeviceMaxWriteImageArgs :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMaxWriteImageArgs = clGetDeviceInfoIntegral Raw.CLDeviceMaxWriteImageArgs

clGetDeviceImage2dMaxWidth :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceImage2dMaxWidth = clGetDeviceInfoIntegral Raw.CLDeviceImage2dMaxWidth

clGetDeviceImage2dMaxHeight :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceImage2dMaxHeight = clGetDeviceInfoIntegral Raw.CLDeviceImage2dMaxHeight

clGetDeviceImage3dMaxWidth :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceImage3dMaxWidth = clGetDeviceInfoIntegral Raw.CLDeviceImage3dMaxWidth

clGetDeviceImage3dMaxHeight :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceImage3dMaxHeight = clGetDeviceInfoIntegral Raw.CLDeviceImage3dMaxHeight

clGetDeviceImage3dMaxDepth :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceImage3dMaxDepth = clGetDeviceInfoIntegral Raw.CLDeviceImage3dMaxDepth

clGetDeviceMaxSamplers :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMaxSamplers = clGetDeviceInfoIntegral Raw.CLDeviceMaxSamplers

clGetDeviceMaxParameterSize :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMaxParameterSize = clGetDeviceInfoIntegral Raw.CLDeviceMaxParameterSize

clGetDeviceMemBaseAddrAlign :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMemBaseAddrAlign = clGetDeviceInfoIntegral Raw.CLDeviceMemBaseAddrAlign

clGetDeviceMinDataTypeAlignSize :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMinDataTypeAlignSize = clGetDeviceInfoIntegral Raw.CLDeviceMinDataTypeAlignSize

clGetDeviceSingleFpConfig :: Raw.CL_device_id -> IO [Raw.CLDeviceFpConfig]
clGetDeviceSingleFpConfig = clGetDeviceInfoBitfield Raw.CLDeviceSingleFpConfig

clGetDeviceGlobalMemCacheType :: Raw.CL_device_id -> IO Raw.CLDeviceMemCacheType
clGetDeviceGlobalMemCacheType = clGetDeviceInfoEnum Raw.CLDeviceGlobalMemCacheType

clGetDeviceGlobalMemCachelineSize :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceGlobalMemCachelineSize = clGetDeviceInfoIntegral Raw.CLDeviceGlobalMemCachelineSize

clGetDeviceGlobalMemCacheSize :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceGlobalMemCacheSize = clGetDeviceInfoIntegral Raw.CLDeviceGlobalMemCacheSize

clGetDeviceGlobalMemSize :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceGlobalMemSize = clGetDeviceInfoIntegral Raw.CLDeviceGlobalMemSize

clGetDeviceMaxConstantBufferSize :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMaxConstantBufferSize = clGetDeviceInfoIntegral Raw.CLDeviceMaxConstantBufferSize

clGetDeviceMaxConstantArgs :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceMaxConstantArgs = clGetDeviceInfoIntegral Raw.CLDeviceMaxConstantArgs

clGetDeviceLocalMemType :: Raw.CL_device_id -> IO Raw.CLDeviceLocalMemType
clGetDeviceLocalMemType = clGetDeviceInfoEnum Raw.CLDeviceLocalMemType

clGetDeviceLocalMemSize :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceLocalMemSize = clGetDeviceInfoIntegral Raw.CLDeviceLocalMemSize

clGetDeviceErrorCorrectionSupport :: Raw.CL_device_id -> IO Raw.CLBool
clGetDeviceErrorCorrectionSupport = clGetDeviceInfoEnum Raw.CLDeviceErrorCorrectionSupport

clGetDeviceProfilingTimerResolution :: (Integral i) => Raw.CL_device_id -> IO i
clGetDeviceProfilingTimerResolution = clGetDeviceInfoIntegral Raw.CLDeviceProfilingTimerResolution

clGetDeviceEndianLittle :: Raw.CL_device_id -> IO Raw.CLBool
clGetDeviceEndianLittle = clGetDeviceInfoEnum Raw.CLDeviceEndianLittle

clGetDeviceAvailable :: Raw.CL_device_id -> IO Raw.CLBool
clGetDeviceAvailable = clGetDeviceInfoEnum Raw.CLDeviceAvailable

clGetDeviceCompilerAvailable :: Raw.CL_device_id -> IO Raw.CLBool
clGetDeviceCompilerAvailable = clGetDeviceInfoEnum Raw.CLDeviceCompilerAvailable

clGetDeviceExecutionCapabilities :: Raw.CL_device_id -> IO [Raw.CLDeviceExecCapabilities]
clGetDeviceExecutionCapabilities = clGetDeviceInfoBitfield Raw.CLDeviceExecutionCapabilities

clGetDeviceQueueProperties :: Raw.CL_device_id -> IO [Raw.CLCommandQueueProperties]
clGetDeviceQueueProperties = clGetDeviceInfoBitfield Raw.CLDeviceQueueProperties

clGetDevicePlatform :: Raw.CL_device_id -> IO Raw.CL_platform_id
clGetDevicePlatform = clGetInfoObject Raw.clGetDeviceInfo Raw.CLDevicePlatform

clGetDeviceName :: Raw.CL_device_id -> IO String
clGetDeviceName = clGetDeviceInfoString Raw.CLDeviceName

clGetDeviceVendor :: Raw.CL_device_id -> IO String
clGetDeviceVendor = clGetDeviceInfoString Raw.CLDeviceVendor

clGetDriverVersion :: Raw.CL_device_id -> IO String
clGetDriverVersion = clGetDeviceInfoString Raw.CLDriverVersion

clGetDeviceProfile :: Raw.CL_device_id -> IO String
clGetDeviceProfile = clGetDeviceInfoString Raw.CLDeviceProfile

clGetDeviceVersion :: Raw.CL_device_id -> IO String
clGetDeviceVersion = clGetDeviceInfoString Raw.CLDeviceVersion

clGetDeviceExtensions :: Raw.CL_device_id -> IO String
clGetDeviceExtensions = clGetDeviceInfoString Raw.CLDeviceExtensions
