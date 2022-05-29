import Lib
import Test.HUnit

testData :: [(Integer, Integer)]
testData =
  [ (1, 299),
    (2, 598),
    (3, 717),
    (4, 956),
    (5, 1195),
    (11, 2409),
    (12, 2628),
    (13, 2847),
    (26, 5174),
    (27, 5373),
    (28, 5572),
    (50, 9950),
    (51, 7599),
    (52, 7748)
  ]

priceTest :: Test
priceTest =
  TestList $
    map
      ( \(n, p) ->
          TestCase
            ( do
                assertEqual
                  ("For " <> show n <> " subscriptions, price should be " <> show p)
                  p
                  (price ranges n)
            )
      )
      testData

main :: IO Counts
main = runTestTT priceTest
