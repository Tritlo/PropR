module Endemic.Search.Class where

import Data.Set (Set)
import Endemic.Configuration.Types (ProblemDescription)
import Endemic.Types (EFix)

class Search conf where
  runRepair :: conf -> ProblemDescription -> IO (Set EFix)