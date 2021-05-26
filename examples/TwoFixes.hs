module TwoFixes where

prop_1 = fst brokenPair == 3

prop_2 = snd brokenPair == 4

prop_3 = a < 5 && b < 5
  where
    (a, b) = brokenPair

brokenPair :: (Int, Int)
brokenPair = (1, 2)