module Parser where

import Control.Applicative
  ( Alternative
  , empty
  , (<|>)
  , some
  , many
  )

import Data.Char
  ( isSpace
  , isDigit
  , isAlpha
  )

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
    [(f a, str') | (a, str') <- parse parser str]

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser $ \str -> [(a, str)]

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  parserF <*> parserA = Parser $ \str ->
    [ (f a, str'')
    | (f, str') <- parse parserF str
    , (a, str'') <- parse parserA str'
    ]

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser $ \str -> []

  -- (<|>) :: Parser a -> Parser a -> Parser a
  parser1 <|> parser2 = Parser $ \str ->
    case parse parser1 str of
      []      -> parse parser2 str
      result  -> result

  -- Below are mutually recursive functions

  -- some :: Parser a -> Parser [a]
  some parser = 
    -- Calls the parser first
    -- and if it is successful (this is the base case)
    -- then calls
    -- it again many times until it fails 
    -- (the failure is defined in `many`)
    (:) <$> parser <*> many parser

  -- many :: Parser a -> Parser [a]
  many parser = 
    -- The first parse can fail (the base case
    -- defined in some)
    -- then if it does, calls the pure []
    some parser <|> pure []

instance Monad Parser where
  -- return :: a -> Parser a
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  parser >>= f = Parser $ \str ->
    case parse parser str of
      []      -> []
      result  -> 
        [ (b, bs)
        | (a, as) <- result
        , (b, bs) <- parse (f a) as
        ]


-- Primitives

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item >>= \c ->
  if f c then return c
         else empty

char :: Char -> Parser Char
char c = satisfy (== c)

spaces :: Parser ()
spaces = do
  many (satisfy isSpace)
  return ()

token :: Parser a -> Parser a
token parser = do
  spaces
  entity <- parser
  spaces
  return entity

nat :: Parser Int
nat = do
  digit <- some (satisfy isDigit)
  return (read digit)

int :: Parser Int
int = (do
  char '-'
  n <- nat
  return (-n)
  ) <|> nat

integer :: Parser Int
integer = token int

arithmetic :: Parser Char
arithmetic = 
  token arithmeticOp
  
  where arithmeticOp = 
              char '+'
          <|> char '-'
          <|> char '*'
          <|> char '/'
