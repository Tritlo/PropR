module SimpleRefinement where

f :: [Int] -> Int
f = foldr (-) 1

-- We want foldl (+) 2 here

prop_isSumPlus2 :: [Int] -> Bool
prop_isSumPlus2 xs = f xs == sum xs + 2

two :: Int
two = 2

main :: IO ()
main = putStrLn "hello, world"

---- EXPECTED ----
-- diff --git a/tests/cases/SimpleRefinement.hs b/tests/cases/SimpleRefinement.hs
-- --- a/tests/cases/SimpleRefinement.hs
-- +++ b/tests/cases/SimpleRefinement.hs
-- @@ -4,1 +4,1 @@ f = foldr (-) 1
-- -f = foldr (-) 1
-- +f = (foldl (+) two)
--
-- diff --git a/tests/cases/SimpleRefinement.hs b/tests/cases/SimpleRefinement.hs
-- --- a/tests/cases/SimpleRefinement.hs
-- +++ b/tests/cases/SimpleRefinement.hs
-- @@ -4,1 +4,1 @@ f = foldr (-) 1
-- -f = foldr (-) 1
-- +f = (foldr (+) two)
---- END EXPECTED ----