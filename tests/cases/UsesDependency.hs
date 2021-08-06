module UsesDependency where

import Dependency

prop_can_use_dependency :: Bool
prop_can_use_dependency = result == 42

result :: Int
result = dependency + 3

one :: Int
one = 1

---- EXPECTED ----
-- diff --git a/tests/cases/UsesDependency.hs b/tests/cases/UsesDependency.hs
-- --- a/tests/cases/UsesDependency.hs
-- +++ b/tests/cases/UsesDependency.hs
-- @@ -9,1 +9,1 @@ result = dependency + 3
-- -result = dependency + 3
-- +result = 42
--
-- diff --git a/tests/cases/UsesDependency.hs b/tests/cases/UsesDependency.hs
-- --- a/tests/cases/UsesDependency.hs
-- +++ b/tests/cases/UsesDependency.hs
-- @@ -9,1 +9,1 @@ result = dependency + 3
-- -result = dependency + 3
-- +result = dependency + (signum (3))
--
-- diff --git a/tests/cases/UsesDependency.hs b/tests/cases/UsesDependency.hs
-- --- a/tests/cases/UsesDependency.hs
-- +++ b/tests/cases/UsesDependency.hs
-- @@ -9,1 +9,1 @@ result = dependency + 3
-- -result = dependency + 3
-- +result = dependency + 1
--
-- diff --git a/tests/cases/UsesDependency.hs b/tests/cases/UsesDependency.hs
-- --- a/tests/cases/UsesDependency.hs
-- +++ b/tests/cases/UsesDependency.hs
-- @@ -9,1 +9,1 @@ result = dependency + 3
-- -result = dependency + 3
-- +result = dependency + one
---- END EXPECTED ----