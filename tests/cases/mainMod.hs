module Main (A (..), main) where

data A = A | B deriving (Eq)

x :: A
x = B

prop_isA :: Bool
prop_isA = x == A

main :: IO ()
main = return ()

---- EXPECTED ----
-- diff --git a/tests/cases/mainMod.hs b/tests/cases/mainMod.hs
-- --- a/tests/cases/mainMod.hs
-- +++ b/tests/cases/mainMod.hs
-- @@ -6,1 +6,1 @@ x = B
-- -x = B
-- +x = A
---- END EXPECTED ----