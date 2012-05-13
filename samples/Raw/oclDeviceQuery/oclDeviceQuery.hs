module Main where

import Foreign.OpenCL.Raw
import Foreign.OpenCL.Raw.C2HS

buf :: Integral a => a
buf = 1024

main = alloca $ \p_num ->
       alloca $ \p_platforms ->
       allocaBytes buf $ \p_param_value ->
       alloca $ \p_param_value_size_ret ->
       alloca $ \p_devices ->
       alloca $ \p_long ->
       do
          clGetPlatformIDs 1
                           p_platforms
                           p_num

          platform <- peek p_platforms

          clGetPlatformInfo platform
                            (cFromEnum CLPlatformName)
                            buf
                            p_param_value
                            p_param_value_size_ret

          platname <- peekCString p_param_value

          clGetDeviceIDs platform
                         (cFromEnum CLDeviceTypeAll)
                         5
                         p_devices
                         p_num

          devices <- peek p_devices

          clGetDeviceInfo devices
                          (cFromEnum CLDeviceName)
                          buf
                          p_param_value
                          p_param_value_size_ret

          device_name <- peekCString p_param_value

          clGetDeviceInfo devices
                          (cFromEnum CLDeviceLocalMemSize)
                          buf
                          (p_long :: Ptr CL_ulong)
                          p_param_value_size_ret

          local_mem_size <- peekIntConv p_long

          clGetDeviceInfo devices
                          (cFromEnum CLDeviceMaxConstantBufferSize)
                          buf
                          p_long
                          p_param_value_size_ret

          buffer_size <- peekIntConv p_long

          putStrLn $ "Platform name: " ++ platname
          putStrLn $ "Device name: " ++ device_name
          putStrLn $ "Local mem size: " ++
            show (local_mem_size)
          putStrLn $ "Constant buffer size: " ++
            show (buffer_size)

          return ()
