module TastyMix where

import Test.Tasty
import Test.Tasty.HUnit (testCase, (@?=))

x :: Int
x = 2

-- We scan mix and match now
prop_bigEnough :: Bool
prop_bigEnough = x > 2

test :: TestTree
test = testCase "Test 1" (x @?= 3)

---- EXPECTED ----
-- diff --git a/tests/cases/TastyMix.hs b/tests/cases/TastyMix.hs
-- --- a/tests/cases/TastyMix.hs
-- +++ b/tests/cases/TastyMix.hs
-- @@ -7,1 +7,1 @@ x = 2
-- -x = 2
-- +x = (succ (2))
--
-- diff --git a/tests/cases/TastyMix.hs b/tests/cases/TastyMix.hs
-- --- a/tests/cases/TastyMix.hs
-- +++ b/tests/cases/TastyMix.hs
-- @@ -7,1 +7,1 @@ x = 2
-- -x = 2
-- +x = 3
---- END EXPECTED ----