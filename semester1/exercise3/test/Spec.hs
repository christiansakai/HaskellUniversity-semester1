import Test.Hspec
import qualified Data.Map.Strict as M
import Lib
  ( Fruit (..)
  , listToBasket
  , getTotalPrice
  , countAppleSpecialOffer
  , countOrangeSpecialOffer
  , getTotalPriceSpecialOffer
  )

main :: IO ()
main = 
  hspec $ do
    describe "listToBasket" $ do
      it "gives the correct basket according to list" $ do
        listToBasket 
          [ Apple
          , Orange
          , Orange
          , Apple
          ] == M.fromList [(Apple, 2), (Orange, 2)]

    describe "getTotalPrice" $ do
      it "gives the correct price according to basket" $ do
        let basket = M.fromList [(Apple, 2), (Orange, 1)]

        getTotalPrice basket == 145

    describe "Special offer count" $ do
      it "by 1 get 1 Apple" $ do
           countAppleSpecialOffer 2 == 1
        && countAppleSpecialOffer 3 == 2
        && countAppleSpecialOffer 04 == 2
        && countAppleSpecialOffer 9 == 5
          
      it "3 for price of 2 Orange" $ do
           countOrangeSpecialOffer 3 == 2
        && countOrangeSpecialOffer 6 == 4
        && countOrangeSpecialOffer 11 == 8 

    describe "Special offer price" $ do
      it "counts price by special offer" $ do
        let basket = M.fromList [(Apple, 2), (Orange, 2)]

        getTotalPriceSpecialOffer basket == 110

  
