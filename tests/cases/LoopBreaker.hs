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

---- EXPECTED ----
-- diff --git a/tests/cases/LoopBreaker.hs b/tests/cases/LoopBreaker.hs
-- --- a/tests/cases/LoopBreaker.hs
-- +++ b/tests/cases/LoopBreaker.hs
-- @@ -7,1 +7,1 @@ x = 5
-- -x = 5
-- +x = (succ 6)
---- END EXPECTED ----