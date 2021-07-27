module Main where

import Test.Tasty
import Test.Tasty.HUnit

y :: Int
y = 12

tests :: TestTree
tests = testCase "Is the answer" (y @?= 42)

main :: IO ()
main = defaultMain tests