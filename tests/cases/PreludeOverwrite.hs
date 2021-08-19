module PreludeOverwrite where

import Prelude hiding (sin)

sin :: Bool
sin = False

prop_sin :: Bool
prop_sin = sin

---- EXPECTED ----
-- diff --git a/tests/cases/PreludeOverwrite.hs b/tests/cases/PreludeOverwrite.hs
-- --- a/tests/cases/PreludeOverwrite.hs
-- +++ b/tests/cases/PreludeOverwrite.hs
-- @@ -6,1 +6,1 @@ sin = False
-- -sin = False
-- +sin = True
--
-- diff --git a/tests/cases/PreludeOverwrite.hs b/tests/cases/PreludeOverwrite.hs
-- --- a/tests/cases/PreludeOverwrite.hs
-- +++ b/tests/cases/PreludeOverwrite.hs
-- @@ -6,1 +6,1 @@ sin = False
-- -sin = False
-- +sin = maxBound
--
-- diff --git a/tests/cases/PreludeOverwrite.hs b/tests/cases/PreludeOverwrite.hs
-- --- a/tests/cases/PreludeOverwrite.hs
-- +++ b/tests/cases/PreludeOverwrite.hs
-- @@ -6,1 +6,1 @@ sin = False
-- -sin = False
-- +sin = otherwise
--
-- diff --git a/tests/cases/PreludeOverwrite.hs b/tests/cases/PreludeOverwrite.hs
-- --- a/tests/cases/PreludeOverwrite.hs
-- +++ b/tests/cases/PreludeOverwrite.hs
-- @@ -6,1 +6,1 @@ sin = False
-- -sin = False
-- +sin = (not (False))
--
-- diff --git a/tests/cases/PreludeOverwrite.hs b/tests/cases/PreludeOverwrite.hs
-- --- a/tests/cases/PreludeOverwrite.hs
-- +++ b/tests/cases/PreludeOverwrite.hs
-- @@ -6,1 +6,1 @@ sin = False
-- -sin = False
-- +sin = (succ (False))
---- END EXPECTED ----