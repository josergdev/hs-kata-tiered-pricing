module Lib where

data Range
  = Finite Integer Integer
  | NegativeInfinite Integer
  | PositiveInfinite Integer
  | Infinite
  deriving (Show)

inRange :: Range -> Integer -> Bool
inRange Infinite _ = True
inRange (NegativeInfinite y) n = n <= y
inRange (PositiveInfinite x) n = n >= x
inRange (Finite x y) n = (x <= n) && (n <= y)

data Tier = Tier
  { range :: Range,
    unitPrice :: Integer
  }
  deriving (Show)

newtype Tiers = Tiers {tiers :: [Tier]} deriving (Show)

ranges :: Tiers
ranges =
  Tiers
    [ Tier (Finite 1 2) 299,
      Tier (Finite 3 10) 239,
      Tier (Finite 11 25) 219,
      Tier (Finite 26 50) 199,
      Tier (PositiveInfinite 51) 149
    ]

search :: Tiers -> Integer -> Tier
search ts n = head $ filter (\t -> (inRange . range) t n) (tiers ts)

price :: Tiers -> Integer -> Integer
price ts n = n * unitPrice (search ts n)