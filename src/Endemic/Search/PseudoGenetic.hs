module Endemic.Search.PseudoGenetic
  ( module Endemic.Search.PseudoGenetic.Search,
    module Endemic.Search.PseudoGenetic.Configuration,
    module Endemic.Search.PseudoGenetic,
  )
where

import Endemic.Search.Class
import Endemic.Search.PseudoGenetic.Configuration
import Endemic.Search.PseudoGenetic.Search

instance Search PseudoGenConf where
  runRepair = pseudoGeneticRepair