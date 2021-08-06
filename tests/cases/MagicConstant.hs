module MagicConstant where

prop_isTheAnswer :: Bool
prop_isTheAnswer = theAnswer == 42

theAnswer :: Int
theAnswer = 17

main :: IO ()
main = print "Unrelated main function"

---- EXPECTED ----
-- diff --git a/tests/cases/MagicConstant.hs b/tests/cases/MagicConstant.hs
-- --- a/tests/cases/MagicConstant.hs
-- +++ b/tests/cases/MagicConstant.hs
-- @@ -7,1 +7,1 @@ theAnswer = 17
-- -theAnswer = 17
-- +theAnswer = 42
---- END EXPECTED ----