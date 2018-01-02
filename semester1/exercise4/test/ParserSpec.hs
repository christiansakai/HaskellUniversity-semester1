module ParserSpec where

import Test.Hspec
import Parser
import Control.Applicative
import Data.Char (isAlpha, isDigit)

test :: IO ()
test = hspec $ do
  lawTest
  primitiveTest

lawTest :: SpecWith ()
lawTest = do
  describe "Parser composition laws" $ do
    it "is an instance of Functor" $ do
      let 
        str = "123"

        parser :: Parser Int
        parser = fmap (\x -> read [x] :: Int) item

        result :: [(Int, String)]
        result = parse parser str

      result `shouldBe` [(1, "23")]

    it "is an instance of Applicative - pure" $ do
      let
        str = "123"

        parser :: Parser Char
        parser = pure 'x'

        result :: [(Char, String)]
        result = parse parser str

      result `shouldBe` [('x', "123")]

    it "is an instance of Applicative - <*>" $ do
      let
        str = "123"

        parserF :: Parser (Char -> String)
        parserF = Parser $ \str -> 
          [(func, str)]
          
          where 
            func :: Char -> String
            func c = "XX" ++ [c] ++ "XX"

        parserB :: Parser String
        parserB = parserF <*> item
     
        result :: [(String, String)]
        result = parse parserB str 

      result `shouldBe` [("XX1XX", "23")]

    it "is an instance of Alternative - empty" $ do
      let
        str = "123"

        result :: [(Char, String)]
        result = parse empty str

      result `shouldBe` []

    it "is an instance of Alternative - <|>" $ do
      let
        str = "123"

        parser :: Parser Char
        parser = empty <|> item

        result :: [(Char, String)]
        result = parse parser str

      result `shouldBe` [('1', "23")]

    it "is an instance of Alternative - some" $ do
      let
        str = "ab123"

        parser :: Parser Char
        parser = Parser $ \str ->
          case str of
            (c:cs) -> if isAlpha c
                         then [(c, cs)]
                         else []
            _      -> []

        result :: [([Char], String)]
        result = parse (some parser) str


      result `shouldBe` [("ab", "123")]

    it "is an instance of Alternative - many" $ do
      let
        str = "123"

        parser :: Parser Char
        parser = Parser $ \str ->
          case str of
            (c:cs) -> if isAlpha c
                         then [(c, cs)]
                         else []
            _      -> []

        result :: [([Char], String)]
        result = parse (many parser) str


      result `shouldBe` [("", "123")]

    it "is an instance of Monad - return" $ do
      let
        str = "123"

        parser :: Parser Char
        parser = do
          return 'x'

        result :: [(Char, String)]
        result = parse parser str

      result `shouldBe` [('x', "123")]

    it "is an instance of Monad - >>=" $ do
      let
        str = "a123b"

        alphabet :: Parser Char
        alphabet = Parser $ \str ->
          case str of
            (c:cs) -> if isAlpha c
                         then [(c, cs)]
                         else []
            _      -> []

        integer :: Parser Char
        integer = Parser $ \str ->
          case str of
            (c:cs) -> if isDigit c
                         then [(c, cs)]
                         else []
            _      -> []

        parser :: Parser Int
        parser = do
          alphabet
          digit <- some integer
          alphabet
          return (read digit)

        result :: [(Int, String)]
        result = parse parser str

      result `shouldBe` [(123, "")]

primitiveTest :: SpecWith ()
primitiveTest =
  describe "Parser primitives" $ do
    it "satisfy creates a parser based on predicate" $ do
      let
        str = "123"

        parser :: Parser Char
        parser = satisfy isDigit

        result :: [(Char, String)]
        result = parse parser str

      result `shouldBe` [('1', "23")]

    it "parses a character" $ do
      let
        str = "a123"
        
        result :: [(Char, String)]
        result = parse (char 'a') str

      result `shouldBe` [('a', "123")]

    it "parses space" $ do
      let
        str = "   123"

        result :: [((), String)]
        result = parse spaces str

      result `shouldBe` [((), "123")]

    it "parses space inbetween" $ do
      let 
        str = "   123    "

        result :: [(String, String)]
        result = parse (token (some (satisfy isDigit))) str

      result `shouldBe` [("123", "")]

    it "parses natural number" $
      parse nat "123" `shouldBe` [(123, "")]

    it "parses positive integer" $ 
      parse int "123" `shouldBe` [(123, "")]

    it "parses negative integer" $ 
      parse int "-123" `shouldBe` [(-123, "")]

    it "parses positive and negative integer with space" $ do
      parse integer "   123  " `shouldBe` [(123 :: Int, "")]
      parse integer " -123   " `shouldBe` [(-123 :: Int, "")]

    it "parses arithmetic operator with space" $ do
      parse arithmetic "+123" `shouldBe` [('+', "123")]
      parse arithmetic "-123" `shouldBe` [('-', "123")]
      parse arithmetic "*123" `shouldBe` [('*', "123")]
      parse arithmetic "/123" `shouldBe` [('/', "123")]


      







