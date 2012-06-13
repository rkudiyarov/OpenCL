module Main where

import System.CPUTime
import System.Environment (getArgs)

import Data.List
import Foreign.OpenCL
import Foreign.Storable
import Foreign.Marshal
import Foreign.Ptr

import qualified Control.Exception as C

import Test

arrayLength :: (Storable a) => [a] -> Int
arrayLength arr = (sizeOf $ head arr) * length arr

lsum [] _ = []
lsum _ [] = []
lsum (xs:x) (ys:y) = (xs + ys):(lsum x y)

unlazyVector l = putStrLn $ show $ last l

main :: IO ()
main = do
  (s1:s2:_) <- getArgs
  let a1 = read s1 :: CL_float
  let a2 = read s2 :: CL_float
  putStrLn $ show a1 ++ " " ++ show a2
  let c = 100000 * a1 :: CL_float
  let a = [1..c]
  let b = map (*2) a
  putStrLn $ show $ last b
  benchmark lsum gpusum "VectorAdd" a2 unlazyVector [(a, b)]
  
gpusum a b = do
     withArray a $ \p_srcA -> do
         withArray b $ \p_srcB -> do
             allocaArray (length a) $ \p_dst ->
                 do
                 pls <- clGetPlatformIDs
                 let pl = head pls
                 dvs <- clGetDeviceIDs pl [CLDeviceTypeGPU]
                 let dv = head dvs
                 ctx <- clCreateContext [] dvs
                 cq  <- clCreateCommandQueue ctx dv []

                 cmDevSrcA <- clCreateBufferFromPtr ctx [CLMemUseHostPtr] (arrayLength a) p_srcA
                 cmDevSrcB <- clCreateBufferFromPtr ctx [CLMemUseHostPtr] (arrayLength b) p_srcB
                 cmDevDst <- clCreateBufferFromPtr ctx [CLMemUseHostPtr] (arrayLength a) (p_dst :: Ptr CL_float)
                 source <- readFile "vectorAdd.cl"
                 pr <- clCreateProgramWithSource ctx [source]
                 clBuildProgram pr dvs []
                 kr <- clCreateKernel pr "VectorAdd"
                 clSetKernelArg kr 0 cmDevSrcA
                 clSetKernelArg kr 1 cmDevSrcB
                 clSetKernelArg kr 2 cmDevDst
                 clSetKernelArg kr 3 (fromIntegral $ length a :: CL_int)

                 st <- getCPUTime
                 clEnqueueNDRangeKernel cq kr [] [(cl_size $ length a)] [1] []
                 et <- getCPUTime

                 clEnqueueReadBufferToPtr cq cmDevDst CLTrue 0 (arrayLength a) p_dst []

                 dst <- peekArray (length a) p_dst
                 
                 clReleaseKernel kr
                 clReleaseProgram pr
                 clReleaseCommandQueue cq
                 clReleaseContext ctx
                 clReleaseMemObject cmDevSrcA
                 clReleaseMemObject cmDevSrcB
                 clReleaseMemObject cmDevDst
                 return (dst, et - st)
