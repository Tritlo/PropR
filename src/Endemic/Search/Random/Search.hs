{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Endemic.Search.Random.Search where

import Control.Arrow (first)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic.Configuration
import Endemic.Repair (checkFixes, findEvaluatedHoles, getHoleFits, processFits, repairAttempt, replacements)
import Endemic.Search.Random.Configuration (RandomConf (..))
import Endemic.Traversals (replaceExpr)
import Endemic.Types
import Endemic.Util
import System.CPUTime (getCPUTime)
import System.Random.SplitMix (SMGen, mkSMGen, nextInteger)

randomRepair :: RandomConf -> ProblemDescription -> IO (Set EFix)
randomRepair r@RandConf {..} desc@ProbDesc {..} = do
  logStr VERBOSE "Starting random search!"
  start <- getCPUTime
  seed <- newSeed
  randomRepair' start (mkSMGen $ fromIntegral seed) Map.empty
  where
    randomRepair' :: Integer -> SMGen -> EFix -> IO (Set EFix)
    randomRepair' start gen fix_so_far = do
      logOut VERBOSE fix_so_far
      cur_time <- getCPUTime
      let diff = cur_time - start
          budget_over = diff >= budgetInPicoSeconds
          fix_too_big = Map.size fix_so_far >= randMaxFixSize
      if budget_over || fix_too_big
        then
          if budget_over
            then logStr INFO "Time budget done, returning!" >> return Set.empty
            else do
              logStr INFO "Fix got too big, starting over!"
              randomRepair' start gen Map.empty
        else do
          let prog' = replaceExpr fix_so_far prog_at_ty
          hole_cands <- collectStats $ findEvaluatedHoles (desc <~ prog')
          let (r_hole_ind, gen') = nextInteger 0 (fromIntegral (length hole_cands - 1)) gen
              chosen_hole = hole_cands !! fromIntegral r_hole_ind
          -- We get a  list of list of fits. However, we assume that there are
          -- no other holes in the program
          -- TODO: Where are the "<interactive>" coming from?
          [fits] <- collectStats $ getHoleFits compConf exprFitCands [chosen_hole] >>= processFits compConf
          let fix_cands :: [(EFix, EExpr)]
              fix_cands = map (first Map.fromList) $ replacements chosen_hole fits
              (r_fix_ind, gen'') = nextInteger 0 (fromIntegral (length fix_cands - 1)) gen'
              (chosen_fix, fixed_prog) = fix_cands !! fromIntegral r_fix_ind
              -- We have to make sure that the chosen_fix takes precedence.
              complete_fix = mergeFixes chosen_fix fix_so_far
          [check_res] <- collectStats $ checkFixes desc [fixed_prog]

          let keep_going = randomRepair' start gen'' complete_fix
              done = do
                logStr INFO "Fix found!"
                logOut INFO complete_fix
                logOut DEBUG fixed_prog
                if randStopOnResults
                  then return $ Set.singleton complete_fix
                  else Set.insert complete_fix <$> randomRepair' start gen'' Map.empty
              try_again = randomRepair' start gen'' fix_so_far

          case check_res of
            Right True -> done
            -- We might want to avoid programs that timeout or fail for some reason.
            Right False ->
              if randIgnoreFailing
                then try_again
                else keep_going
            Left res -> if and res then done else keep_going

    efcs = Just exprFitCands
    EProb {..} = progProblem
    prog_at_ty = progAtTy e_prog e_ty
    budgetInPicoSeconds = fromIntegral randSearchBudget * 1_000_000_000_000
