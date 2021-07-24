module TastyTwoFix where

import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

wrong_pair :: (Int, Int)
wrong_pair = (1, 2)

test :: TestTree
test =
  testGroup
    "twofix"
    [ testCase "Test 1" (fst wrong_pair @?= 3),
      testCase "Test 2" (snd wrong_pair @?= 4),
      testCase "Test 3 (should work)" (uncurry (<) wrong_pair @?= True)
    ]
