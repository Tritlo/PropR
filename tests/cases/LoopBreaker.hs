module LoopBreaker where

prop_is_7 :: Bool
prop_is_7 = x == succ 6

x :: Int
x = 5

-- This function will loop
y :: [Int] -> Int
y = ($!) y

g :: Int
g = y []
