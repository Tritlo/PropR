module PropR.Search.PseudoGenetic
  ( module PropR.Search.PseudoGenetic.Search,
    module PropR.Search.PseudoGenetic.Configuration,
    module PropR.Search.PseudoGenetic,
  )
where

import PropR.Search.Class
import PropR.Search.PseudoGenetic.Configuration
import PropR.Search.PseudoGenetic.Search

instance Search PseudoGenConf where
  runRepair = pseudoGeneticRepair
