module Lib where

data Range = Range
  { min :: Maybe Integer,
    max :: Maybe Integer
  }
  deriving (Show)

rangeMin :: Integer -> Range
rangeMin a = Range (Just a) Nothing

rangeMinMax :: Integer -> Integer -> Range
rangeMinMax a b = Range (Just a) (Just b)

inRange :: Range -> Integer -> Bool
inRange (Range Nothing Nothing) n = True
inRange (Range Nothing (Just y)) n = n <= y
inRange (Range (Just x) Nothing) n = n >= x
inRange (Range (Just x) (Just y)) n = (x <= n) && (n <= y)

data Tier = Tier
  { range :: Range,
    unitPrice :: Integer
  }
  deriving (Show)

newtype TieredPricing = TieredPricing {tiers :: [Tier]}

tierFor :: TieredPricing -> Integer -> Tier
tierFor tp n = head $ filter (\t -> (inRange . range) t n) (tiers tp)

price :: TieredPricing -> Integer -> Integer
price tp n = n * unitPrice (tierFor tp n)

ranges :: TieredPricing
ranges =
  TieredPricing
    [ Tier (rangeMinMax 1 2) 299,
      Tier (rangeMinMax 3 10) 239,
      Tier (rangeMinMax 11 25) 219,
      Tier (rangeMinMax 26 50) 199,
      Tier (rangeMin 51) 149
    ]