module TwoFixes where

prop_1 = fst brokenPair == 3

prop_2 = snd brokenPair == 4

prop_3 = a < 5 && b < 5
  where
    (a, b) = brokenPair

brokenPair :: (Int, Int)
brokenPair = (1, 2)

-- We had an error where the context was not being passed into the trace, so
-- this would result in an out-of-scope error:
fiveInTwoFixes :: Int
fiveInTwoFixes = 7