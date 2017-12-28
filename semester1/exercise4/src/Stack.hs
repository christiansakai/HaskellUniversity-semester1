module Stack 
  ( Stack
  , size
  , push
  , empty
  , isEmpty
  , pop
  ) where

import Data.Maybe

data Stack a = Stack 
    { content :: [a]
    , size :: Int
    } deriving Show

empty :: Stack a
empty = Stack [] 0

isEmpty :: Stack a -> Bool
isEmpty (Stack [] 0)  = True
isEmpty _             = False

push :: a -> Stack a -> Stack a
push x (Stack xs size) = Stack (x:xs) (size + 1)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [] size)     = (Nothing, Stack [] size)
pop (Stack (x:xs) size) = (Just x, Stack xs (size - 1))
