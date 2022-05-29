module Lib where

data Range
  = Finite Integer Integer
  | NegativeInfinity Integer
  | PositiveInfinity Integer
  | Infinity
  deriving (Show)

inRange :: Range -> Integer -> Bool
inRange Infinity n = True
inRange (NegativeInfinity y) n = n <= y
inRange (PositiveInfinity x) n = n >= x
inRange (Finite x y) n = (x <= n) && (n <= y)

data Tier = Tier
  { range :: Range,
    unitPrice :: Integer
  }
  deriving (Show)

newtype TieredPricing = TieredPricing {tiers :: [Tier]} deriving (Show)

tierFor :: TieredPricing -> Integer -> Tier
tierFor tp n = head $ filter (\t -> (inRange . range) t n) (tiers tp)

price :: TieredPricing -> Integer -> Integer
price tp n = n * unitPrice (tierFor tp n)

ranges :: TieredPricing
ranges =
  TieredPricing
    [ Tier (Finite 1 2) 299,
      Tier (Finite 3 10) 239,
      Tier (Finite 11 25) 219,
      Tier (Finite 26 50) 199,
      Tier (PositiveInfinity 51) 149
    ]