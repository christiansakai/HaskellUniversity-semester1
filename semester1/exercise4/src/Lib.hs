module Lib
    ( someFunc
    , evalStack
    ) where

import qualified Stack as S
import qualified Parser as P

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- parseExpr :: String -> P.Parser (S.Stack Expr)
-- parseExpr str = P.parse undefined str

evalStack :: S.Stack Int -> Int
evalStack stack = 
  case fst (S.pop stack) of
    Nothing -> 0
    Just n  -> n



