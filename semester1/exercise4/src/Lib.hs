module Lib where

import Parser
import Control.Applicative 
  (some, (<|>)) 

-- parseString :: String -> IO ()
-- parseString str = do
--   (result, "") <- parse polishParser str

--   case result of
--     Left err -> 
--       print err
--     Right expr ->
--       print $ eval expr

-- pol :: Parser Int
-- pol = do
--   result <- int <|> arithmetic
--   case result of
--     '-' ->


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

  -- if (length exprs) - 1 /= (length ops)
  --   then return $ Left "expression error"
  --   else
  return $ 
    Right (combine (reverse exprs) ops)

  where
    combine :: [Expr] -> [Expr -> Expr -> Expr] -> Expr
    combine [e] []            = e 
    combine [e, e'] [o]       = o e' e
    combine (e:e':es) (o:os)  = 
      let es' = (o e' e):es
          os' = os
       in combine es' os'

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
