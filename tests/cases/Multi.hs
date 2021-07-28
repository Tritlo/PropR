module Multi where

import Test.Tasty
import Test.Tasty.HUnit

y :: Int
y = 20

x :: Int
x = 25

some_nums :: (Int, Int)
some_nums = (30, 12)

tests :: TestTree
tests = testCase "Is the answer" (x + y @?= 42)

prop_XIs30 :: Bool
prop_XIs30 = x == 30

main :: IO ()
main = defaultMain tests
