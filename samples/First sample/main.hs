module Main where

import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal
import Foreign.OpenCL.Raw.V10.Types
import Foreign.OpenCL.Raw.V10.Device
import Foreign.OpenCL.Raw.V10.Platform

main = alloca $ \hm -> alloca $ \pip -> do
          pl <- clGetPlatformIDs 1 hm
          dev <- peek hm
          plIn <- clGetPlatformInfo dev CLPlatformName 1024 pip
          hmm <- peekCString pip
          putStrLn hmm
          return ()