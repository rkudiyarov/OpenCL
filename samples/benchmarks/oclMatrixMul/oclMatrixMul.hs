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

makeMatrix _ [] = []
makeMatrix n xs = (take n xs):(makeMatrix n $ drop n xs)

arrayLength :: (Storable a) => [a] -> Int
arrayLength arr = (sizeOf $ head arr) * length arr

matrixLength x = (fromIntegral $ round $ sqrt $ realToFrac $ toRational (fromIntegral $ length $ x :: CL_int) :: Int)

matrixMul [] _ = []
matrixMul _ [] = []
matrixMul x y = mul_matr (makeMatrix (matrixLength x) x) ( transpose (makeMatrix (matrixLength y) y) )
    where
      mul_vect _ [] = 0
      mul_vect [] _ = 0
      mul_vect (x:xs) (y:ys) = x*y + mul_vect xs ys
      mul_matr xs ys = [ [ mul_vect x y | y <- ys ] | x <- xs ]

unlazyMatrix l = putStrLn $ show $ last $ last l

main :: IO ()
main = do
  (s1:s2:_) <- getArgs
  let a1 = read s1 :: CL_float
  let a2 = read s2 :: CL_float
  putStrLn $ show a1 ++ " " ++ show a2
  let c = 10 * a1 :: CL_float
  let a = [1..c * c]
  let b = map (*2) a
  putStrLn $ show $ last b
  benchmark matrixMul matrixMulGpu "MatrixMul" a2 unlazyMatrix [(a, b)]

matrixMulGpu a b = do
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

                 source <- readFile "matrixMul.cl"
                 pr <- clCreateProgramWithSource ctx [source]
                 clBuildProgram pr dvs []
                 kr <- clCreateKernel pr "MatrixMul"
                 clSetKernelArg kr 0 cmDevSrcA
                 clSetKernelArg kr 1 cmDevSrcB
                 clSetKernelArg kr 2 cmDevDst
                 let msize = cl_int $ matrixLength a :: CL_int
                 clSetKernelArg kr 3 msize
                 clSetKernelArg kr 4 msize

                 st <- getCPUTime
                 clEnqueueNDRangeKernel cq kr [] [(cl_size $ msize), (cl_size $ msize)] [1, 1] []
                 et <- getCPUTime

                 clEnqueueReadBufferToPtr cq cmDevDst CLTrue 0 (arrayLength a) p_dst []

                 dst <- peekArray (length a) p_dst

                 start_time10 <- getCPUTime
                 putStrLn $ show $ last $ last $ makeMatrix (matrixLength dst) dst
                 start_time11 <- getCPUTime

                 clReleaseKernel kr
                 clReleaseProgram pr
                 clReleaseCommandQueue cq
                 clReleaseContext ctx
                 clReleaseMemObject cmDevSrcA
                 clReleaseMemObject cmDevSrcB
                 clReleaseMemObject cmDevDst
                 return (dst, et - st + (start_time11 - start_time10))
