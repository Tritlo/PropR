-- |
-- Module      : Endemic.Search.Class
-- Description : Provides a Top-Level Interface for search algorithms
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- This module defines a TopLevel Class to define Search Algorithms, that is dependent on a (parameterized) Configuration.
-- The class hence pushes all responsibilities towards the algorithms and enables the rest of the program to use them "plug-and-play".
-- For example, the Genetic Search provides an interface to runRepair given a GeneticConfiguration. 
-- All the logic the Genetic Search requires is fully handeled by the genetic repair, including calling functions for fixes etc. 
-- Towards the higher level functions, all other pieces can use the repair exchangeably and just need a proper setup of the configurations.

module Endemic.Search.Class where

import Data.Set (Set)
import Endemic.Configuration.Types (ProblemDescription)
import Endemic.Types (EFix)

class Search conf where
  runRepair :: conf -> ProblemDescription -> IO (Set EFix)