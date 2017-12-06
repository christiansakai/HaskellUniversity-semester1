module Main where

import Lib

main :: IO ()
main = do
  line <- getLine  
  let list :: [String]
      list = filter (/= "") . words $ line

      numList :: [Int]
      numList = fmap read list

  print $ convertListToString (quickSort numList)

convertListToString :: Show a => [a] -> String
convertListToString []     = []
convertListToString (x:xs) = (show x) ++ " " ++ (convertListToString xs)

