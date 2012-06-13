module Test where

import System.CPUTime
import Data.List

sc = 1000000000.0

average l = (fromIntegral (sum l)) / (fromIntegral (length l))

showResults [] = ""
showResults ((c, t, mn, mx):ls) = show c ++ "\t" ++ (show $ (t / sc)) ++ "\t" ++ (show $ (mn / sc)) ++ "\t" ++ (show $ (mx / sc)) ++ "\n" ++ showResults ls

benchmark cpu gpu fn rpt u cases = do
  results <- benchmarkCases cpu gpu rpt u cases
  appendFile (fn ++ ".txt") $ showResults results

benchmarkCases _ _ _ _ [] = return []
benchmarkCases cpu gpu rpt u (c:cs) = do
  r <- benchmarkCase cpu gpu rpt u c
  rs <- benchmarkCases cpu gpu rpt u cs
  return (r:rs)
  
benchmarkCase cpu gpu rpt u c = do
  let a = fst c
  rs <- benchmarkRepeat cpu gpu a (snd c) u rpt
  let cpuTime = map (\(x, _, _) -> x) rs 
  let fullGpuTime = map (\(_, x, _) -> x) rs
  let gpuTime = map (\(_, _, x) -> x) rs
  return (length a, average cpuTime, average fullGpuTime, average gpuTime)

benchmarkRepeat _ _ _ _ _ 0 = return []
benchmarkRepeat cpu gpu a b u rpt = do
  r <- makeBenchmark cpu gpu a b u
  rs <- benchmarkRepeat cpu gpu a b u (rpt - 1)
  return (r:rs)

makeBenchmark cpu gpu a b u = do
  stcpu <- getCPUTime
  u $ cpu a b
  etcpu <- getCPUTime
  (_, tgpu) <- gpu a b
  etgpu <- getCPUTime
  return (etcpu - stcpu, etgpu - etcpu, tgpu)