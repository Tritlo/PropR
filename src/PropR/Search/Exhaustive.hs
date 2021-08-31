module PropR.Search.Exhaustive
  ( module PropR.Search.Exhaustive.Search,
    module PropR.Search.Exhaustive.Configuration,
    module PropR.Search.Exhaustive,
  )
where

import PropR.Search.Class
import PropR.Search.Exhaustive.Configuration
import PropR.Search.Exhaustive.Search

instance Search ExhaustiveConf where
  runRepair = exhaustiveRepair
