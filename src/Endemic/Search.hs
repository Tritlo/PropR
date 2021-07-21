module Endemic.Search
  ( module Endemic.Search.Genetic,
    module Endemic.Search.PseudoGenetic,
    module Endemic.Search.Class,
  )
where

import Endemic.Configuration (SearchAlgorithm (..))
import Endemic.Search.Class
import Endemic.Search.Genetic
import Endemic.Search.PseudoGenetic

instance Search SearchAlgorithm where
  runRepair (Genetic c) = runRepair c
  runRepair (PseudoGenetic c) = runRepair c