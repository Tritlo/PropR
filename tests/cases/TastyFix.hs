module TastyFix where

import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

x :: Int
x = 2

test :: TestTree
test = testCase "Test 1" (x @?= 3)
