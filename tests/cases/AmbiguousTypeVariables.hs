module AmbiguousTypeVariables where

import Data.List (nub)

isOkay :: [[Maybe Int]] -> Bool
isOkay sudoku = and [isOkayBlock b | b <- blocks sudoku]

blocks :: [[Maybe Int]] -> [[Maybe Int]]
blocks = id

isOkayBlock :: [Maybe Int] -> Bool
isOkayBlock b = length b == length (nub b)

prop_anyBlockBlah :: Int -> Bool
prop_anyBlockBlah r
  | r < 0 = True
  | r == 9 = True
  | otherwise = not (isOkayBlock [Nothing | _ <- [0 .. r]])

---- EXPECTED ----
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = (negate (length b)) == length (nub b)
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = 0 == length (nub b)
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length (Left (b)) == length (nub b)
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length (head (b)) == length (nub b)
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length (init (b)) == length (nub b)
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length (maximum (b)) == length (nub b)
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length (minimum (b)) == length (nub b)
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length (sequence (b)) == length (nub b)
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length (sequenceA (b)) == length (nub b)
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length (sequence_ (b)) == length (nub b)
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length [] == length (nub b)
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == 0
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == length (Left ((nub b)))
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == length (init ((nub b)))
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == length (maximum ((nub b)))
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == length (minimum ((nub b)))
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == length (sequence ((nub b)))
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == length (sequenceA ((nub b)))
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == length Nothing
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == length []
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == length (nub (init (b)))
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == length (nub (show (b)))
--
-- diff --git a/tests/cases/AmbiguousTypeVariables.hs b/tests/cases/AmbiguousTypeVariables.hs
-- --- a/tests/cases/AmbiguousTypeVariables.hs
-- +++ b/tests/cases/AmbiguousTypeVariables.hs
-- @@ -12,1 +12,1 @@ isOkayBlock b = length b == le...
-- -isOkayBlock b = length b == length (nub b)
-- +isOkayBlock b = length b == length (nub (tail (b)))
---- END EXPECTED ----