module Lib where

range :: Maybe Integer -> Maybe Integer -> Integer -> Bool
range Nothing Nothing _ = True
range Nothing (Just y) n = n <= y
range (Just x) Nothing n = n >= x
range (Just x) (Just y) n = (x <= n) && (n <= y)

price :: [(Integer -> Bool, Integer)] -> Integer -> Integer
price rs n = n * snd (head $ filter (\(f, p) -> f n) rs)

ranges :: [(Integer -> Bool, Integer)]
ranges =
  [ (range (Just 1) (Just 2), 299),
    (range (Just 3) (Just 10), 239),
    (range (Just 11) (Just 25), 219),
    (range (Just 26) (Just 50), 199),
    (range (Just 51) Nothing, 149)
  ]