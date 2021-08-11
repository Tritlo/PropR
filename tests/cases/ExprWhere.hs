module ExprWhere where

example :: [Maybe Int]
example = [n]
  where
    n = Nothing

allBlankEx :: [Maybe Int]
allBlankEx = [Just 1]

propIsAllBlank :: Bool
propIsAllBlank = example == allBlankEx

---- EXPECTED ----
-- diff --git a/tests/cases/ExprWhere.hs b/tests/cases/ExprWhere.hs
-- --- a/tests/cases/ExprWhere.hs
-- +++ b/tests/cases/ExprWhere.hs
-- @@ -4,4 +4,4 @@ example
--  example
-- -  = [n]
-- +  = (mempty ([n]))
--    where
--        n = Nothing
-- @@ -9,1 +9,1 @@ allBlankEx = [Just 1]
-- -allBlankEx = [Just 1]
-- +allBlankEx = (tail ([Just 1]))
--
-- diff --git a/tests/cases/ExprWhere.hs b/tests/cases/ExprWhere.hs
-- --- a/tests/cases/ExprWhere.hs
-- +++ b/tests/cases/ExprWhere.hs
-- @@ -4,4 +4,4 @@ example
--  example
-- -  = [n]
-- +  = [Just 1]
--    where
--        n = Nothing
--
-- diff --git a/tests/cases/ExprWhere.hs b/tests/cases/ExprWhere.hs
-- --- a/tests/cases/ExprWhere.hs
-- +++ b/tests/cases/ExprWhere.hs
-- @@ -4,4 +4,4 @@ example
--  example
-- -  = [n]
-- +  = allBlankEx
--    where
--        n = Nothing
--
-- diff --git a/tests/cases/ExprWhere.hs b/tests/cases/ExprWhere.hs
-- --- a/tests/cases/ExprWhere.hs
-- +++ b/tests/cases/ExprWhere.hs
-- @@ -4,4 +4,4 @@ example
--  example
-- -  = [n]
-- +  = [(Just 1)]
--    where
--        n = Nothing
--
-- diff --git a/tests/cases/ExprWhere.hs b/tests/cases/ExprWhere.hs
-- --- a/tests/cases/ExprWhere.hs
-- +++ b/tests/cases/ExprWhere.hs
-- @@ -9,1 +9,1 @@ allBlankEx = [Just 1]
-- -allBlankEx = [Just 1]
-- +allBlankEx = example
--
-- diff --git a/tests/cases/ExprWhere.hs b/tests/cases/ExprWhere.hs
-- --- a/tests/cases/ExprWhere.hs
-- +++ b/tests/cases/ExprWhere.hs
-- @@ -9,1 +9,1 @@ allBlankEx = [Just 1]
-- -allBlankEx = [Just 1]
-- +allBlankEx = [Nothing]
---- END EXPECTED ----