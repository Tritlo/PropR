module TastyFix where

import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

x :: Int
x = 2

test :: TestTree
test = testCase "Test 1" (x @?= 3)

---- EXPECTED ----
-- diff --git a/tests/cases/TastyFix.hs b/tests/cases/TastyFix.hs
-- --- a/tests/cases/TastyFix.hs
-- +++ b/tests/cases/TastyFix.hs
-- @@ -7,1 +7,1 @@ x = 2
-- -x = 2
-- +x = (succ (2))
--
-- diff --git a/tests/cases/TastyFix.hs b/tests/cases/TastyFix.hs
-- --- a/tests/cases/TastyFix.hs
-- +++ b/tests/cases/TastyFix.hs
-- @@ -7,1 +7,1 @@ x = 2
-- -x = 2
-- +x = 3
---- END EXPECTED ----