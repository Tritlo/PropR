module Endemic.Search
  ( module Endemic.Search.Genetic,
    module Endemic.Search.PseudoGenetic,
    module Endemic.Search.Random,
    module Endemic.Search.Class,
  )
where

import Endemic.Configuration (SearchAlgorithm (..))
import Endemic.Search.Class
import Endemic.Search.Exhaustive
import Endemic.Search.Genetic
import Endemic.Search.PseudoGenetic
import Endemic.Search.Random

instance Search SearchAlgorithm where
  runRepair (Genetic c) = runRepair c
  runRepair (PseudoGenetic c) = runRepair c
  runRepair (Random c) = runRepair c
  runRepair (Exhaustive c) = runRepair c