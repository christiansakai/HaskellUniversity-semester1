module Lib
    ( program1
    , program2
    , program3
    ) where

program1 :: IO ()
program1 = putStrLn "Hello World"

program2 :: IO ()
program2 = do
  putStrLn "Hello, what is your name?"
  name <- getLine
  putStrLn $ "Nice to meet you " ++ name

program3 :: IO ()
program3 = do
  program2
  program3
