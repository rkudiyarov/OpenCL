module Main where

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.OpenCL.Raw.Internal.Types
import Foreign.OpenCL.Raw.V10.Device
import Foreign.OpenCL.Raw.V10.Platform

main = alloca $ \hm -> do
          pl <- getPlatformIDs 1 hm
          dev <- peek hm
          plIn <- getPlatformInfo pip 2306
          putStrLn $ show hm
          return ()