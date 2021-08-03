module Main (A (..), main) where

data A = A | B deriving (Eq)

x :: A
x = B

prop_isA :: Bool
prop_isA = x == A

main :: IO ()
main = return ()