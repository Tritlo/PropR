module Issue88 where

import Data.List (transpose)

rows :: [[Int]] -> [[Int]]
rows = transpose

rowSum2 :: [[Int]] -> [Int]
rowSum2 xs = map product xs

f :: [[Int]] -> [Int]
f rows = map product (take 3 rows)

prop_rowSum2IsOk :: [[Int]] -> Bool
prop_rowSum2IsOk xs = map sum xs == rowSum2 xs

---- EXPECTED ----
-- diff --git a/tests/cases/Issue88.hs b/tests/cases/Issue88.hs
-- --- a/tests/cases/Issue88.hs
-- +++ b/tests/cases/Issue88.hs
-- @@ -9,1 +9,1 @@ rowSum2 xs = map product xs
-- -rowSum2 xs = map product xs
-- +rowSum2 xs = map sum xs
---- END EXPECTED ----