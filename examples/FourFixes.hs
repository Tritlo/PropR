module FourFixes where

prop_1 = a == 3
  where
    (a, _, _, _) = brokenPair

prop_2 = b == 4
  where
    (_, b, _, _) = brokenPair

prop_3 = c == 5
  where
    (_, _, c, _) = brokenPair

prop_4 = d == 6
  where
    (_, _, _, d) = brokenPair

prop_5 = a < 5 && b < 5 && c > 0 && d > 0
  where
    (a, b, c, d) = brokenPair

brokenPair :: (Int, Int, Int, Int)
brokenPair = (1, 2, 3, 4)