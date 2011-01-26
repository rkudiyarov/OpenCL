module Main where

import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal
import Foreign.OpenCL.Raw.V10.Types
import Foreign.OpenCL.Raw.V10.Platform
import Foreign.OpenCL.Raw.V10.Device
import Foreign.OpenCL.Raw.C2HS

main = alloca $ \p_num_platforms ->
       alloca $ \p_platforms ->
       alloca $ \p_param_value ->
       alloca $ \p_param_value_size_ret ->
       alloca $ \p_devices ->
       alloca $ \p_num_devices ->
       alloca $ \p_d_param_value ->
       alloca $ \p_d_param_value_size_ret ->
       do
          clGetPlatformIDs 1
                           p_platforms
                           p_num_platforms
          platform <- peek p_platforms
          clGetPlatformInfo platform
                            (clPlatformInfo CLPlatformName)
                            1024
                            p_param_value
                            p_param_value_size_ret
          platname <- peekCString p_param_value
          putStrLn $ "Platform name: " ++ platname
          clGetDeviceIDs platform
                         (clDeviceType CLDeviceTypeAll)
                         5
                         p_devices
                         p_num_devices
          devices <- peek p_devices
          clGetDeviceInfo devices
                          (clDeviceInfo CLDeviceType)
                          1024
                          (p_d_param_value :: Ptr CLDeviceType)
                          p_d_param_value_size_ret
          str_device_type <- peekEnum p_d_param_value
          putStrLn $ "Device type: " ++
            show (cToEnum str_device_type :: CLDDeviceType)
          return ()
