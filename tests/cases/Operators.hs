module Operators where

(++++) :: Int -> Int -> Int
(++++) = (-)

prop_isPlus :: Bool
prop_isPlus = 1 ++++ 2 == 3
