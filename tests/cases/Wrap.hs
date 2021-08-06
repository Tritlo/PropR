module Wrap where

onlyGoodOnes :: [Int] -> [Int]
onlyGoodOnes = filter even

xs :: [Int]
xs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

prop_allEven :: Bool
prop_allEven = all even xs && not (null xs)

---- EXPECTED ----
-- diff --git a/tests/cases/Wrap.hs b/tests/cases/Wrap.hs
-- --- a/tests/cases/Wrap.hs
-- +++ b/tests/cases/Wrap.hs
-- @@ -7,1 +7,1 @@ xs = [1, 2, 3, 4, 5, 6, 7, 8, ...
-- -xs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
-- +xs = ((filter even) ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
--
-- diff --git a/tests/cases/Wrap.hs b/tests/cases/Wrap.hs
-- --- a/tests/cases/Wrap.hs
-- +++ b/tests/cases/Wrap.hs
-- @@ -7,1 +7,1 @@ xs = [1, 2, 3, 4, 5, 6, 7, 8, ...
-- -xs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
-- +xs = (onlyGoodOnes ([1, 2, 3, 4, 5, 6, 7, 8, 9, 10]))
---- END EXPECTED ----