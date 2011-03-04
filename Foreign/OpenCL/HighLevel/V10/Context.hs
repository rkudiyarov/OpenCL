--------------------------------------------------------------------------------
-- |
-- Copyright : (c) [2011] Vadim Zakondyrin
-- License   : BSD
-- |
--------------------------------------------------------------------------------

module Foreign.OpenCL.HighLevel.V10.Context
       ( CLContextProperties
--       , clCreateContext
--       , clCreateContextFromType
       , clRetainContext
       , clReleaseContext
       , clGetContextReferenceCount
       , clGetContextDevices
       )
       where

import qualified Foreign.OpenCL.Raw.V10 as Raw
import Foreign.OpenCL.Raw.C2HS

import Foreign.OpenCL.HighLevel.V10.Error
import Foreign.OpenCL.HighLevel.V10.Utils

data CLContextProperties = CLContextPlatform Raw.CL_platform_id

-- TODO: to add a callback function with user data to clCreateContext and clCreateContextFromType

--clCreateContext :: [CLContextProperties] -> [Raw.CL_device_id] -> IO Raw.CL_context

--clCreateContextFromType :: [CLContextProperties] -> [Raw.CLDeviceType] -> IO Raw.CL_context

clRetainContext :: Raw.CL_context -> IO ()
clRetainContext = clRetain Raw.clRetainContext

clReleaseContext :: Raw.CL_context -> IO ()
clReleaseContext = clRelease Raw.clReleaseContext

clGetContextReferenceCount :: (Integral i) => Raw.CL_context -> IO i
clGetContextReferenceCount = clGetInfoIntegral Raw.clGetContextInfo Raw.CLContextReferenceCount

clGetContextDevices :: Raw.CL_context -> IO [Raw.CL_device_id]
clGetContextDevices = clGetInfoObjectsArray Raw.clGetContextInfo Raw.CLContextDevices