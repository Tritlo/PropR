module TastyMix where

import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

x :: Int
x = 2

-- We scan mix and match now
prop_bigEnough :: Bool
prop_bigEnough = x > 2

test :: TestTree
test = testCase "Test 1" (x @?= 3)