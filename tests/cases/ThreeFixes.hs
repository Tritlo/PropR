module ThreeFixes where

prop_1 = a == 3
  where
    (a, _, _) = brokenPair

prop_2 = b == 4
  where
    (_, b, _) = brokenPair

prop_3 = c == 5
  where
    (_, _, c) = brokenPair

prop_4 = a < 5 && b < 5 && c > 0
  where
    (a, b, c) = brokenPair

brokenPair :: (Int, Int, Int)
brokenPair = (1, 2, 3)

sixInThreeFixes :: Int
sixInThreeFixes = 6
