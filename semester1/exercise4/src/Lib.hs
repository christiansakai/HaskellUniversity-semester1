module Lib where

-- import Control.Applicative (some)
-- import qualified Parser as P
-- import qualified Stack as S

-- polishParser :: P.Parser Int
-- polishParser = do
--   operands <- some P.integer
--   operator <- P.arithmeticOp

--   let left = head operands
--       right = last operands

--   case operator of
--     '+' -> return $ left + right 
--     '-' -> return $ left - right 
--     '*' -> return $ left * right 
--     '/' -> return $ left `div` right 
