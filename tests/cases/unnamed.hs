prop_is5 :: Bool
prop_is5 = x == 5

x :: Int
x = 4

---- EXPECTED ----
-- diff --git a/tests/cases/unnamed.hs b/tests/cases/unnamed.hs
-- --- a/tests/cases/unnamed.hs
-- +++ b/tests/cases/unnamed.hs
-- @@ -5,1 +5,1 @@ x = 4
-- -x = 4
-- +x = 5
--
-- diff --git a/tests/cases/unnamed.hs b/tests/cases/unnamed.hs
-- --- a/tests/cases/unnamed.hs
-- +++ b/tests/cases/unnamed.hs
-- @@ -5,1 +5,1 @@ x = 4
-- -x = 4
-- +x = (succ (4))
---- END EXPECTED ----