module Lib
    ( quickSort
    , fizzBuzz
    ) where

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (x:xs) = 
  let pivot = x
      lessEqualThanPivot = [ s | s <- xs, s <= pivot ]
      biggerThanPivot = [ b | b <- xs, b > pivot ]
   in quickSort(lessEqualThanPivot) 
      ++ [pivot] 
      ++ quickSort(biggerThanPivot)

fizzBuzz :: Int -> [String]
fizzBuzz n 
  | n < 0     = []
  | otherwise = fmap fizzBuzzify [1..n] 

fizzBuzzify :: Int -> String
fizzBuzzify n
  | n `mod` 3 == 0  = "Fizz"
  | n `mod` 5 == 0  = "Buzz"
  | n `mod` 15 == 0 = "FizzBuzz"
  | otherwise       = show n
  

