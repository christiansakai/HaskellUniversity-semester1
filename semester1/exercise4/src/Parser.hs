module Parser
  ( Parser
  , parse
  , integer
  , arithmeticOp
  ) where

import Control.Applicative
  ( Alternative
  , empty
  , (<|>)
  , some
  , many
  )
import Control.Monad
  ( MonadPlus
  , mzero
  , mplus
  )
import Data.Char 
  ( isDigit
  , isSpace
  )


-- Basic Type

newtype Parser a = Parser { parse :: String -> [(a, String)] }

item :: Parser Char
item = Parser $ \str ->
  case str of
    ""      -> []
    (c:cs)  -> [(c, cs)]


-- Composition Laws

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parser = Parser $ \str ->
    case parse parser str of
      []        -> []
      [(c, cs)] -> [(f c, cs)]

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure c = Parser $ \str -> [(c, str)]

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  parserF <*> parserA = Parser $ \str ->
    case parse parserF str of
      []          -> []
      [(f, str')] -> parse (fmap f parserA) str'

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser $ \str -> [] 

  -- <|> :: Parser a -> Parser a -> Parser a
  parser <|> parser' = Parser $ \str ->
    case parse parser str of
      []        -> parse parser' str
      [(c, cs)] -> [(c, cs)]
 
  -- some :: Parser a -> Parser [a]
  some parser = pure (:) <*> parser <*> many parser

  -- many :: Parser a -> Parser [a]
  many parser = some parser <|> pure []

instance Monad Parser where
  -- return :: a -> Parser a
  return = pure

  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  parser >>= f = Parser $ \str ->
    case parse parser str of
      []        -> []
      [(c, cs)] -> parse (f c) cs

instance MonadPlus Parser where
  -- mzero :: Parser a
  mzero = empty

  -- mplus :: Parser a -> Parser a -> Parser a
  parser `mplus` parser' = Parser $ \str ->
    (parse parser str ++ parse parser' str)

-- Primitives

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item >>= \c ->
  if f c then return c
         else empty

digit :: Parser Char
digit = satisfy isDigit

space :: Parser Char
space = satisfy isSpace

char :: Char -> Parser Char
char c = satisfy (== c)

nat :: Parser Int
nat = do
  cs <- some digit
  return (read cs)

spaces :: Parser ()
spaces = do
  many space
  return ()

int :: Parser Int
int = (do
  char '-'
  n <- nat
  return (-n)
  ) <|> nat

token :: Parser a -> Parser a
token parser = do
  spaces
  cs <- parser
  spaces
  return (cs)

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: Char -> Parser Char
symbol sym = token (char sym)

arithmeticOp :: Parser Char
arithmeticOp = symbol '+'
           <|> symbol '-'
           <|> symbol '*'
           <|> symbol '/'

