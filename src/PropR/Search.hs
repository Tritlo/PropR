module PropR.Search
  ( module PropR.Search.Genetic,
    module PropR.Search.PseudoGenetic,
    module PropR.Search.Random,
    module PropR.Search.Class,
  )
where

import PropR.Configuration (SearchAlgorithm (..))
import PropR.Search.Class
import PropR.Search.Exhaustive
import PropR.Search.Genetic
import PropR.Search.PseudoGenetic
import PropR.Search.Random

instance Search SearchAlgorithm where
  runRepair (Genetic c) = runRepair c
  runRepair (PseudoGenetic c) = runRepair c
  runRepair (Random c) = runRepair c
  runRepair (Exhaustive c) = runRepair c
