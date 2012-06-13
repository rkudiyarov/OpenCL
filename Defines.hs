module Main where

import Data.Char

parseFile :: String -> [[String]]
parseFile f = filter (\x -> x /= []) [words y | y <- lines f, y /= []]

processFile :: String -> String
processFile = createFile . ppFile . filter (\x -> head x == "#define") . parseFile

createFile :: [String] -> String
createFile = unlines . map (\x -> "    " ++ x)

ppFile :: [[String]] -> [String]
ppFile = map processLine

processLine :: [String] -> String
processLine (_:xs:_) = customise xs

myWords :: String -> [String]
myWords s          =  case dropWhile (\x -> x == '_') s of
                      "" -> []
                      s' -> w : myWords s''
                            where (w, s'') = break (\x -> x == '_') s'

customise :: String -> String
customise xs = hsWord (myWords xs) ++ " = " ++ xs ++ ","

hsWord :: [String] -> String
hsWord (w:ws) = w ++ hsWord' ws
       where hsWord' [] = ""
             hsWord' ((t:ts):xs) = t:(map toLower ts) ++ hsWord' xs

main :: IO ()
main = do putStrLn "Enter path to cl.h"
          path_to_file <- getLine
          putStrLn "Enter path to output file"
          path_to_new_file <- getLine
          fl <- readFile path_to_file
          writeFile path_to_new_file $ processFile fl
