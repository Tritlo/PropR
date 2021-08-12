module Issue87 where

import Data.List (transpose)

rows :: [[Int]] -> [[Int]]
rows = transpose

-- Fails with function fits enabled
rowSum :: [[Int]] -> [Int]
rowSum rows = map product rows

prop_rowSumIsOk :: [[Int]] -> Bool
prop_rowSumIsOk xs = map sum xs == rowSum xs

---- EXPECTED ----
-- diff --git a/tests/cases/Issue87.hs b/tests/cases/Issue87.hs
-- --- a/tests/cases/Issue87.hs
-- +++ b/tests/cases/Issue87.hs
-- @@ -10,1 +10,1 @@ rowSum rows = map product rows
-- -rowSum rows = map product rows
-- +rowSum rows = map sum rows
---- END EXPECTED ----