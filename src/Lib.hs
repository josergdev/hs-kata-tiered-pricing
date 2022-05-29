module Lib where

data Range = Range
  { min :: Maybe Integer,
    max :: Maybe Integer
  }
  deriving (Show)

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
    [ Tier (Range (Just 1) (Just 2)) 299,
      Tier (Range (Just 3) (Just 10)) 239,
      Tier (Range (Just 11) (Just 25)) 219,
      Tier (Range (Just 26) (Just 50)) 199,
      Tier (Range (Just 51) Nothing) 149
    ]