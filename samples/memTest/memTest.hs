module Main where

import Foreign.OpenCL.V10
import Foreign.C.Types

main :: IO ()
main = do
       pls <- clGetPlatformIDs
       let pl = head pls
       dvs <- clGetDeviceIDs pl [CLDeviceTypeGPU]
       c <- clCreateContext [ContextPlatform pl] dvs
       let buf = [3.5, 11.5, 32.2, 34.7, 15.1] :: [CDouble]
       m <- clCreateBufferFromList c [CLMemReadWrite, CLMemCopyHostPtr] buf
       list <- clGetMemObjectList m :: IO [CDouble]
       object_type <- clGetMemObjectType m
       clReleaseMemObject m
       clReleaseContext c
       putStrLn $ "List of data:  " ++ show list
       putStrLn $ "MemObjectType: " ++ show object_type