module PropR.Search.Random
  ( module PropR.Search.Random.Search,
    module PropR.Search.Random.Configuration,
    module PropR.Search.Random,
  )
where

import PropR.Search.Class
import PropR.Search.Random.Configuration
import PropR.Search.Random.Search

instance Search RandomConf where
  runRepair = randomRepair
