{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Endemic.Search.Exhaustive.Search where

import Control.Arrow (first, (***))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic.Configuration.Types (ProblemDescription (..), (<~))
import Endemic.Repair (repairAttempt)
import Endemic.Search.Exhaustive.Configuration (ExhaustiveConf (..), Unmaterialized (umExhaustiveSearchBudget))
import Endemic.Traversals (replaceExpr)
import Endemic.Types
import Endemic.Util
import System.CPUTime (getCPUTime)

exhaustiveRepair :: ExhaustiveConf -> ProblemDescription -> IO (Set EFix)
exhaustiveRepair r@ExhaustiveConf {..} desc@ProbDesc {..} = do
  start <- getCPUTime
  -- We use BFS, i.e. check all 1 level fixes, then all 2 level fixes, etc:
  logStr VERBOSE "Starting exhaustive search!"
  let loop unfixed = do
        (fixes, to_check') <- reduce <$> mapM (exhaustiveRepairIter start) unfixed
        if not (Set.null fixes)
          then do
            logStr INFO "Found fixes!"
            mapM_ (logOut INFO) $ Set.toList fixes
            if exhStopOnResults
              then return fixes
              else Set.union fixes <$> loop to_check'
          else loop to_check'
  loop [Map.empty]
  where
    reduce :: [(Set EFix, [EFix])] -> (Set EFix, [EFix])
    reduce = (Set.unions *** List.concat) . unzip
    ProbDesc {progProblem = EProb {e_prog = start_prog}} = desc
    exhaustiveRepairIter :: Integer -> EFix -> IO (Set EFix, [EFix])
    exhaustiveRepairIter start cur_fix = do
      logOut VERBOSE cur_fix
      cur_time <- getCPUTime
      let diff = cur_time - start
          budget_over = diff >= budgetInPicoSeconds
      if budget_over
        then logStr INFO "Time budget done, returning!" >> return (Set.empty, [])
        else do
          let to_cur_fix = first (`mergeFixes` cur_fix)
          all_fixes <-
            collectStats $
              map to_cur_fix
                <$> repairAttempt (desc <~ replaceExpr cur_fix prog_at_ty)
          let isFixed (Right x) = x
              isFixed (Left ps) = and ps
              (fixed, not_fixed) = List.partition (isFixed . snd) all_fixes
              to_check =
                map fst $
                  if exhIgnoreFailing
                    then List.filter ((/= Right False) . snd) not_fixed
                    else not_fixed
              found_fixes = Set.fromList (map fst fixed)
          return (found_fixes, to_check)
    EProb {..} = progProblem
    prog_at_ty = progAtTy e_prog e_ty
    budgetInPicoSeconds = fromIntegral exhSearchBudget * 1_000_000_000_000
