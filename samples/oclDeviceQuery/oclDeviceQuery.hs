module Main where

import Foreign.OpenCL.V10
--import qualified Foreign.OpenCL.Raw.V10.Enums as Raw

showDeviceInfo a = do
                   dv_name <- clGetDeviceName a
                   dv_type <- clGetDeviceType a
                   dv_vendor <- clGetDeviceVendor a
                   dv_pl <- clGetDevicePlatform a
                   dv_dims <- clGetDeviceMaxWorkItemDimensions a
                   dv_sizes <- clGetDeviceMaxWorkItemSizes a
                   dv_im_sup <- clGetDeviceImageSupport a
                   dv_max_samp <- clGetDeviceMaxSamplers a
                   dv_fp_cfg <- clGetDeviceSingleFpConfig a
                   dv_mem_type <- clGetDeviceGlobalMemCacheType a
                   dv_exec_cap <- clGetDeviceExecutionCapabilities a
                   dv_que_prop <- clGetDeviceQueueProperties a
                   putStr $ "-----------------\n"
                   putStr $ "Device Name:    \t" ++ show dv_name ++ "\n"
                   putStr $ "Device Type:    \t" ++ show dv_type ++ "\n"
                   putStr $ "Device Vendor   \t" ++ show dv_vendor ++ "\n"
                   putStr $ "Device Platform:\t" ++ show dv_pl ++ "\n"
                   putStr $ "Device Max Dims:\t" ++ show dv_dims ++ "\n"
                   putStr $ "Device Max Sizes:\t" ++ show dv_sizes ++ "\n"
                   putStr $ "Device Im Supports:\t" ++ show dv_im_sup ++ "\n"
                   putStr $ "Device Max Samplers:\t" ++ show dv_max_samp ++ "\n"
                   putStr $ "Device Fp Config:\t" ++ show dv_fp_cfg ++ "\n"
                   putStr $ "Device Gl Mem Type:\t" ++ show dv_mem_type ++ "\n"
                   putStr $ "Device Exec Cap:\t" ++ show dv_exec_cap ++ "\n"
                   putStr $ "Device Queue Prop:\t" ++ show dv_que_prop ++ "\n"
                   putStr $ "-----------------\n"

showPlatformInfo a = do
                     pl_name <- clGetPlatformName a
                     pl_version <- clGetPlatformVersion a
                     pl_vendor <- clGetPlatformVendor a
                     pl_ext <- clGetPlatformExtensions a
                     pl_profile <- clGetPlatformProfile a
                     devices <- clGetDeviceIDs [CLDeviceTypeAll] a
                     putStr $ "----------------------------\n"
                     putStr $ "Platform:        \t" ++ show a ++ "\n"
                     putStr $ "Platform Name:   \t" ++ show pl_name ++ "\n"
                     putStr $ "Platform Version:\t" ++ show pl_version ++ "\n"
                     putStr $ "Platform Vendor: \t" ++ show pl_vendor ++ "\n"
                     putStr $ "Platform Profile:\t" ++ show pl_profile ++ "\n"
                     putStr $ "Platform Ext:    \t" ++ show pl_ext ++ "\n"
                     putStr $ "Devices Info:\n"
                     mapM (showDeviceInfo) devices
                     putStr $ "----------------------------\n\n"

main = do platforms <- clGetPlatformIDs
          mapM (showPlatformInfo) platforms
