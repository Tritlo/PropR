module Endemic.Search.Exhaustive
  ( module Endemic.Search.Exhaustive.Search,
    module Endemic.Search.Exhaustive.Configuration,
    module Endemic.Search.Exhaustive,
  )
where

import Endemic.Search.Class
import Endemic.Search.Exhaustive.Configuration
import Endemic.Search.Exhaustive.Search

instance Search ExhaustiveConf where
  runRepair = exhaustiveRepair
