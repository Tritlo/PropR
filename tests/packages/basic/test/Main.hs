import Test.Tasty
import Test.Tasty.QuickCheck

import Ex
main :: IO ()
main =
  defaultMainWithIngredients defaultIngredients $
  testGroup "mid"
  [ testProperty "sum"           $ \xs -> foldInt (+) 0 xs == sum xs,
    testProperty "product"       $ \xs -> foldInt (*) 1 xs == product xs,
    testProperty "negate $ sum"  $ \xs -> foldInt (-) 0 xs == negate (sum xs)]

