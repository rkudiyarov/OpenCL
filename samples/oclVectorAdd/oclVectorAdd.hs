module Main where

import Foreign.OpenCL.V10
import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr

maxL :: CL_float
maxL = 10

srcA :: [CL_float]
srcA = [i | i <- [1..maxL]]
srcB :: [CL_float]
srcB = [2*i | i <- [1..maxL]]

arrayLength :: (Storable a) => [a] -> Int
arrayLength arr = (sizeOf $ head arr) * length arr

main :: IO ()
main =
     withArray srcA $ \p_srcA ->
         withArray srcB $ \p_srcB ->
             allocaArray (length srcA) $ \p_dst ->
                 do
                 pls <- clGetPlatformIDs
                 let pl = head pls
                 dvs <- clGetDeviceIDs pl [CLDeviceTypeAll]
                 let dv = head dvs
                 ctx <- clCreateContext [] dvs
                 cq  <- clCreateCommandQueue ctx dv []
                 cmDevSrcA <- clCreateBufferFromPtr ctx [CLMemReadOnly] (arrayLength srcA) p_srcA
                 cmDevSrcB <- clCreateBufferFromPtr ctx [CLMemReadOnly] (arrayLength srcB) p_srcB
                 cmDevDst <- clCreateBufferFromPtr ctx [CLMemWriteOnly] (arrayLength srcA) (p_dst :: Ptr CL_float)
                 source <- readFile "vectorAdd.cl"
                 pr <- clCreateProgramWithSource ctx [source]
                 clBuildProgram pr dvs []
                 kr <- clCreateKernel pr "VectorAdd"
                 clSetKernelArg kr 0 cmDevSrcA
                 clSetKernelArg kr 1 cmDevSrcB
                 clSetKernelArg kr 2 cmDevDst
                 clSetKernelArg kr 3 $ length srcA
                 clEnqueueWriteBufferFromPtr cq cmDevSrcA CLFalse 0 (arrayLength srcA) p_srcA []
                 clEnqueueWriteBufferFromPtr cq cmDevSrcB CLFalse 0 (arrayLength srcB) p_srcB []
                 clEnqueueNDRangeKernel cq kr [] [(cl_size $ length srcA)] [] []
                 clEnqueueReadBufferToPtr cq cmDevDst CLTrue 0 (arrayLength srcA) p_dst []

                 dst <- peekArray (length srcA) p_dst

                 putStrLn $ "Array A: " ++ show srcA
                 putStrLn $ "Array B: " ++ show srcB
                 putStrLn $ "Array C: " ++ show dst

                 clReleaseKernel kr
                 clReleaseProgram pr
                 clReleaseCommandQueue cq
                 clReleaseContext ctx
                 clReleaseMemObject cmDevSrcA
                 clReleaseMemObject cmDevSrcB
                 clReleaseMemObject cmDevDst
                 return ()