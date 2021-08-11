module DefaultingFixes where

import Data.Maybe (isJust)

allBlankEx :: [Maybe Int]
allBlankEx = [Just 2 | n <- [0]]

propIsAllBlank :: Bool
propIsAllBlank = not (any isJust allBlankEx)

---- EXPECTED ----
-- diff --git a/tests/cases/DefaultingFixes.hs b/tests/cases/DefaultingFixes.hs
-- --- a/tests/cases/DefaultingFixes.hs
-- +++ b/tests/cases/DefaultingFixes.hs
-- @@ -6,1 +6,1 @@ allBlankEx = [Just 2 | n <- [0]]
-- -allBlankEx = [Just 2 | n <- [0]]
-- +allBlankEx = (mempty ([Just 2 | n <- [0]]))
--
-- diff --git a/tests/cases/DefaultingFixes.hs b/tests/cases/DefaultingFixes.hs
-- --- a/tests/cases/DefaultingFixes.hs
-- +++ b/tests/cases/DefaultingFixes.hs
-- @@ -6,1 +6,1 @@ allBlankEx = [Just 2 | n <- [0]]
-- -allBlankEx = [Just 2 | n <- [0]]
-- +allBlankEx = (tail ([Just 2 | n <- [0]]))
--
-- diff --git a/tests/cases/DefaultingFixes.hs b/tests/cases/DefaultingFixes.hs
-- --- a/tests/cases/DefaultingFixes.hs
-- +++ b/tests/cases/DefaultingFixes.hs
-- @@ -6,1 +6,1 @@ allBlankEx = [Just 2 | n <- [0]]
-- -allBlankEx = [Just 2 | n <- [0]]
-- +allBlankEx = []
--
-- diff --git a/tests/cases/DefaultingFixes.hs b/tests/cases/DefaultingFixes.hs
-- --- a/tests/cases/DefaultingFixes.hs
-- +++ b/tests/cases/DefaultingFixes.hs
-- @@ -6,1 +6,1 @@ allBlankEx = [Just 2 | n <- [0]]
-- -allBlankEx = [Just 2 | n <- [0]]
-- +allBlankEx = mempty
--
-- diff --git a/tests/cases/DefaultingFixes.hs b/tests/cases/DefaultingFixes.hs
-- --- a/tests/cases/DefaultingFixes.hs
-- +++ b/tests/cases/DefaultingFixes.hs
-- @@ -6,1 +6,1 @@ allBlankEx = [Just 2 | n <- [0]]
-- -allBlankEx = [Just 2 | n <- [0]]
-- +allBlankEx = [Nothing | n <- [0]]
--
-- diff --git a/tests/cases/DefaultingFixes.hs b/tests/cases/DefaultingFixes.hs
-- --- a/tests/cases/DefaultingFixes.hs
-- +++ b/tests/cases/DefaultingFixes.hs
-- @@ -6,1 +6,1 @@ allBlankEx = [Just 2 | n <- [0]]
-- -allBlankEx = [Just 2 | n <- [0]]
-- +allBlankEx = [Just 2 | n <- (init ([0]))]
--
-- diff --git a/tests/cases/DefaultingFixes.hs b/tests/cases/DefaultingFixes.hs
-- --- a/tests/cases/DefaultingFixes.hs
-- +++ b/tests/cases/DefaultingFixes.hs
-- @@ -6,1 +6,1 @@ allBlankEx = [Just 2 | n <- [0]]
-- -allBlankEx = [Just 2 | n <- [0]]
-- +allBlankEx = [Just 2 | n <- (mempty ([0]))]
--
-- diff --git a/tests/cases/DefaultingFixes.hs b/tests/cases/DefaultingFixes.hs
-- --- a/tests/cases/DefaultingFixes.hs
-- +++ b/tests/cases/DefaultingFixes.hs
-- @@ -6,1 +6,1 @@ allBlankEx = [Just 2 | n <- [0]]
-- -allBlankEx = [Just 2 | n <- [0]]
-- +allBlankEx = [Just 2 | n <- (tail ([0]))]
--
-- diff --git a/tests/cases/DefaultingFixes.hs b/tests/cases/DefaultingFixes.hs
-- --- a/tests/cases/DefaultingFixes.hs
-- +++ b/tests/cases/DefaultingFixes.hs
-- @@ -6,1 +6,1 @@ allBlankEx = [Just 2 | n <- [0]]
-- -allBlankEx = [Just 2 | n <- [0]]
-- +allBlankEx = [Just 2 | n <- []]
--
-- diff --git a/tests/cases/DefaultingFixes.hs b/tests/cases/DefaultingFixes.hs
-- --- a/tests/cases/DefaultingFixes.hs
-- +++ b/tests/cases/DefaultingFixes.hs
-- @@ -6,1 +6,1 @@ allBlankEx = [Just 2 | n <- [0]]
-- -allBlankEx = [Just 2 | n <- [0]]
-- +allBlankEx = [Just 2 | n <- mempty]
---- END EXPECTED ----