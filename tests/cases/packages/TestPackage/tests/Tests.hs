module Main where

import Test.Package
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testCase "Is the answer" (x @?= 42)

main :: IO ()
main = defaultMain tests