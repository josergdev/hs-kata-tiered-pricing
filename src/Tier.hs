module Tier
  ( tiers,
    price,
  )
where

type SubscriptionAmount = Integer

data Range
  = Finite SubscriptionAmount SubscriptionAmount
  | NegativeInfinite SubscriptionAmount
  | PositiveInfinite SubscriptionAmount
  | Infinite
  deriving (Show)

type Price = Integer

data Tier = Tier
  { range :: Range,
    unitPrice :: Price
  }
  deriving (Show)

type Tiers = [Tier]

inRange :: Range -> SubscriptionAmount -> Bool
inRange Infinite _ = True
inRange (NegativeInfinite y) n = n <= y
inRange (PositiveInfinite x) n = n >= x
inRange (Finite x y) n = (x <= n) && (n <= y)

tiers :: Tiers
tiers =
  [ Tier (Finite 1 2) 299,
    Tier (Finite 3 10) 239,
    Tier (Finite 11 25) 219,
    Tier (Finite 26 50) 199,
    Tier (PositiveInfinite 51) 149
  ]

search :: Tiers -> SubscriptionAmount -> Tier
search ts n = head $ filter (\t -> (inRange . range) t n) ts

price :: Tiers -> SubscriptionAmount -> Price
price ts n = n * unitPrice (search ts n)