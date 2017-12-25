module Lib
  ( Fruit (..)
  , listToBasket
  , getTotalPrice
  , countAppleSpecialOffer
  , countOrangeSpecialOffer
  , getTotalPriceSpecialOffer
  ) where

import qualified Data.Map.Strict as M

data Fruit
  = Apple
  | Orange
  deriving (Show, Eq, Ord)

type Basket = M.Map Fruit Int

type BasketSpecialOffer = 
  M.Map Fruit (Int -> Int)

getPrice :: Fruit -> Int
getPrice Apple  = 60
getPrice Orange = 25

listToBasket :: [Fruit] -> Basket
listToBasket fs =
  foldl putToBasket emptyBasket fs
    where
      emptyBasket = M.fromList [(Apple, 0), (Orange, 0)]
      putToBasket acc el = 
        M.adjustWithKey (\el -> \val -> val + 1) el acc

getTotalPrice :: Basket -> Int
getTotalPrice basket = 
  M.foldlWithKey accumulatePrice 0 basket
    where
      accumulatePrice acc el val = acc + (getPrice el * val)

countAppleSpecialOffer :: Int -> Int
countAppleSpecialOffer appleCount = 
  ceiling (toRational appleCount / 2)

countOrangeSpecialOffer :: Int -> Int
countOrangeSpecialOffer orangeCount = 
  (ceiling (toRational orangeCount / 3)) * 2

getTotalPriceSpecialOffer :: Basket -> Int
getTotalPriceSpecialOffer basket =
  foldl countPriceSpecial 0 listAmount
    where 
      listAmount = M.toList basket
      countPriceSpecial acc (Apple, amount) =
        acc + (countAppleSpecialOffer amount * getPrice Apple)
      countPriceSpecial acc (Orange, amount) =
        acc + (countOrangeSpecialOffer amount * getPrice Orange)


