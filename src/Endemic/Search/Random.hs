module Endemic.Search.Random
  ( module Endemic.Search.Random.Search,
    module Endemic.Search.Random.Configuration,
    module Endemic.Search.Random,
  )
where

import Endemic.Search.Class
import Endemic.Search.Random.Configuration
import Endemic.Search.Random.Search

instance Search RandomConf where
  runRepair = randomRepair
