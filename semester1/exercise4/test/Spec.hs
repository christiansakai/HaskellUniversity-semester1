import Test.Hspec

import qualified Test.ParserSpec as Parser
-- import qualified Stack as S 
-- import qualified Parser as P

main :: IO ()
main = do
  Parser.test

-- stackTest :: IO ()
-- stackTest = hspec $ do
--   describe "Stack" $ do
--     it "gives an empty stack" $ do
--       let stack = S.empty 

--       S.isEmpty stack == True 
--         && S.size stack == 0

--     it "push an element to the stack" $ do
--       let stack = S.empty
--           stack' = S.push 1 stack

--       S.isEmpty stack' == False
--         && S.size stack' == 1

--     it "pop an element from the stack" $ do
--       let stack = S.empty
--           stack' = S.push 1 stack
--           (el, stack'') = S.pop stack'

--       S.isEmpty stack'' == True
--         && el == Just 1
--         && S.size stack'' == 0

-- parserTest :: IO ()
-- parserTest = hspec $ do
--   describe "Parser" $ do
--     it "parses positive integers" $
--       P.parse P.integer "123abc" == [(123, "abc")]

--     it "parses negative integers" $ 
--       P.parse P.integer "-123abac" == [(-123, "abac")]

--     it "parses + operator" $ 
--       P.parse P.arithmeticOp "+abc" == [('+', "abc")]

--     it "parses - operator" $ do
--       P.parse P.arithmeticOp "- 123" == [('-', "123")]

--     it "parses * operator" $ do
--       P.parse P.arithmeticOp "*abc" == [('*', "abc")]

--     it "parses / operator" $ do
--       P.parse P.arithmeticOp "/123" == [('/', "123")]
