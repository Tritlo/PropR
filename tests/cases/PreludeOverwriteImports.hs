module PreludeOverwriteImports where

import PrelOverwriteExports
import Prelude hiding (sin)

is_sin :: Bool
is_sin = not sin

prop_sin :: Bool
prop_sin = is_sin

---- EXPECTED ----
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = True
--
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = maxBound
--
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = otherwise
--
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = sin
--
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = not (not sin)
--
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = not False
--
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = not minBound
--
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = not prop_sin
--
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = (not (not sin))
--
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = (succ (not sin))
--
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = not (not (sin))
--
-- diff --git a/tests/cases/PreludeOverwriteImports.hs b/tests/cases/PreludeOverwriteImports.hs
-- --- a/tests/cases/PreludeOverwriteImports.hs
-- +++ b/tests/cases/PreludeOverwriteImports.hs
-- @@ -7,1 +7,1 @@ is_sin = not sin
-- -is_sin = not sin
-- +is_sin = not (pred (sin))
---- END EXPECTED ----
