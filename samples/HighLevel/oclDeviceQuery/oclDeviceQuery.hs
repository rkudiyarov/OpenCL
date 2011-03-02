module Main where

import Foreign.OpenCL.HighLevel.V10

showPlatformInfo a = do
                     pl_name <- clGetPlatformName a
                     pl_version <- clGetPlatformVersion a
                     putStr $ "Platform Name: " ++ pl_name ++ "\n"
                     putStr $ "Platform Version: " ++ pl_version ++ "\n"

main = do platforms <- clGetPlatformIDs
          mapM (showPlatformInfo) platforms
