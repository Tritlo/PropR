module DoubleDecl where

x, y :: Int
x = 32
y = 17

prop_xIs42 :: Bool
prop_xIs42 = x == 42

prop_yIs42 :: Bool
prop_yIs42 = y == 42

---- EXPECTED ----
-- diff --git a/tests/cases/DoubleDecl.hs b/tests/cases/DoubleDecl.hs
-- --- a/tests/cases/DoubleDecl.hs
-- +++ b/tests/cases/DoubleDecl.hs
-- @@ -4,1 +4,1 @@ x = 32
-- -x = 32
-- +x = 42
-- @@ -5,1 +5,1 @@ y = 17
-- -y = 17
-- +y = 42
---- END EXPECTED ----