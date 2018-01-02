module Lib where

import Parser
import Control.Applicative 
  (some, (<|>)) 

data Expr
  = Lit Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving Show

eval :: Expr -> Int
eval (Lit i)            = i
eval (Add expr1 expr2)  = eval expr1 + eval expr2
eval (Sub expr1 expr2)  = eval expr1 - eval expr2
eval (Mul expr1 expr2)  = eval expr1 * eval expr2
eval (Div expr1 expr2)  = eval expr1 `div` eval expr2

polishParser :: Parser (Either String Expr)
polishParser = do
  exprs <- some parseInt
  ops <- some parseOp

  if (length exprs) - 1 /= (length ops)
    then return $ Left "expression error"
    else
      let revExprs = reverse exprs
          revOps = reverse ops

      in return $ Right (combine revExprs revOps)

  where
    combine :: [Expr] -> [Expr -> Expr -> Expr] -> Expr
    combine [expr] []             = expr
    combine (expr:exprs) (op:ops) = op expr (combine exprs ops)
  
parseInt :: Parser Expr
parseInt = do
  lit <- integer
  return $ Lit lit

parseOp :: Parser (Expr -> Expr -> Expr)
parseOp = do
  operator <- arithmetic

  return $
    case operator of
      '+' -> Add
      '-' -> Sub
      '*' -> Mul
      '/' -> Div
