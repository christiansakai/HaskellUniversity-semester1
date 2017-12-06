import Test.QuickCheck
import Data.List.Utils (startswith, endswith)
import Lib (quickSort, fizzBuzz)

main :: IO ()
main = do
  quickCheck prop_quickSort
  quickCheck prop_fizzBuzz
  
prop_quickSort :: [Int] -> Bool
prop_quickSort list = 
  quickSort(quickSort(list)) == quickSort(list)

prop_fizzBuzz :: Int -> Bool
prop_fizzBuzz n 
  | n <= 0 = True
  | otherwise = all checker indexed 
                where 
                  fizzBuzzed = fizzBuzz n
                  indexed = zip [1..] fizzBuzzed
                  checker (i, el)
                    | i `mod` 3 == 0 = startswith "Fizz" el
                    | i `mod` 5 == 0 = endswith "Buzz" el
                    | otherwise      = True

