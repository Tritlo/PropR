module BrokenModule where

prop_isSum :: [Int] -> Bool
prop_isSum xs = broken xs == sum xs

broken :: [Int] -> Int
broken = foldl (-) 0

add :: Int -> Int -> Int
add = (+)

main :: IO ()
main = print "Unrelated main function"

---- EXPECTED ----
-- diff --git a/tests/cases/BrokenModule.hs b/tests/cases/BrokenModule.hs
-- --- a/tests/cases/BrokenModule.hs
-- +++ b/tests/cases/BrokenModule.hs
-- @@ -7,1 +7,1 @@ broken = foldl (-) 0
-- -broken = foldl (-) 0
-- +broken = sum
--
-- diff --git a/tests/cases/BrokenModule.hs b/tests/cases/BrokenModule.hs
-- --- a/tests/cases/BrokenModule.hs
-- +++ b/tests/cases/BrokenModule.hs
-- @@ -7,1 +7,1 @@ broken = foldl (-) 0
-- -broken = foldl (-) 0
-- +broken = foldl (+) 0
--
-- diff --git a/tests/cases/BrokenModule.hs b/tests/cases/BrokenModule.hs
-- --- a/tests/cases/BrokenModule.hs
-- +++ b/tests/cases/BrokenModule.hs
-- @@ -7,1 +7,1 @@ broken = foldl (-) 0
-- -broken = foldl (-) 0
-- +broken = foldl add 0
---- END EXPECTED ----