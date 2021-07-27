module UsesDependency where

import Dependency

prop_can_use_dependency :: Bool
prop_can_use_dependency = result == 42

result :: Int
result = dependency + 3

one :: Int
one = 1