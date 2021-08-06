module Operators where

(++++) :: Int -> Int -> Int
(++++) = (-)

prop_isPlus :: Bool
prop_isPlus = 1 ++++ 2 == 3

---- EXPECTED ----
-- diff --git a/tests/cases/Operators.hs b/tests/cases/Operators.hs
-- --- a/tests/cases/Operators.hs
-- +++ b/tests/cases/Operators.hs
-- @@ -4,1 +4,1 @@ (++++) = (-)
-- -(++++) = (-)
-- +(++++) = (+)
---- END EXPECTED ----