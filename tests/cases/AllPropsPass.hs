module AllPropsPass where

import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

x :: Int
x = 2

-- We scan mix and match now
prop_bigEnough :: Bool
prop_bigEnough = x >= 2

prop_mul2Is4 :: Bool
prop_mul2Is4 = x * 2 == 4

tests =
  [ testCase "Test 1" (x + 2 @?= 4),
    testCase "Test 2" (x @?= 4)
  ]

test :: TestTree
test = testCase "Test 1" (x @?= 3)