module Main where

import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal
import Foreign.OpenCL.Raw.V10.Types
import Foreign.OpenCL.Raw.V10.Device
import Foreign.OpenCL.Raw.V10.Platform
import Foreign.OpenCL.Raw.C2HS

main = alloca $ \hm -> alloca $ \pip -> alloca $ \di -> alloca $ \dinfo -> do
          pl <- clGetPlatformIDs 1 hm
          dev <- peek hm
          plIn <- clGetPlatformInfo dev CLPlatformName 1024 pip
          platname <- peekCString pip
          putStrLn $ "Platform name: " ++ platname
          clGetDeviceIDs dev CLDeviceTypeGPU 5 di
          peekDi <- peek di
          testa <- clGetDeviceInfo peekDi CLDeviceType 1024 (dinfo :: Ptr Int)
--          en <- peekIntConv dinfo           
          lastDinfo <- peekEnum dinfo :: IO CLDeviceType
          putStrLn $ "Device type: " ++ show lastDinfo
          return ()

--peekDeviceInfo :: (Storable a, Integral a) => a -> IO (CLDeviceInfo)
--peekDeviceInfo ptr = do pic <- peekIntConv ptr
--                        return $ cToEnum pic