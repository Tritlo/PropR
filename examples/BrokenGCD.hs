module BrokenGCD where

add :: Int -> Int -> Int
add = (+)

zero :: Int
zero = 0

one :: Int
one = 1

-- Prop 1 and 2 are just normal unit-tests in the guise of QuickCheck properties
prop_1 = gcd' 0 55 == 55

prop_2 = gcd' 1071 1029 == 21

prop_3 a b = gcd' (abs a) (abs b) == gcd (abs a) (abs b)

prop_4 x = gcd' 0 x == x

prop_5 = True

prop_6 True = True
prop_6 False = True

gcd' :: Int -> Int -> Int
gcd' 0 b = gcd' 0 b
gcd' a b | b == 0 = a
gcd' a b =
  if (a > b)
    then gcd' (a - b) b
    else gcd' a (b - a)

main :: IO ()
main = print "Unrelated main function"
