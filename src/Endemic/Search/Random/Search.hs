{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

module Endemic.Search.Random.Search where

import Control.Arrow (first)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic.Configuration (ProblemDescription (..), newSeed, (~>))
import Endemic.Eval
import Endemic.Repair (checkFixes, findEvaluatedHoles, getHoleFits, processFits, replacements)
import Endemic.Search.Random.Configuration (RandomConf (..))
import Endemic.Traversals (replaceExpr)
import Endemic.Types
import Endemic.Util
import GHC (HsExpr (..), NoExtField (NoExtField))
import GhcPlugins
import System.CPUTime (getCPUTime)
import System.Random.SplitMix (SMGen, mkSMGen, nextInteger)

randomRepair :: RandomConf -> ProblemDescription -> IO (Set EFix)
randomRepair r@RandConf {..} desc@ProbDesc {..} = do
  start <- liftIO getCPUTime
  seed <- newSeed
  randomRepair' start (mkSMGen $ fromIntegral seed) Map.empty
  where
    randomRepair' :: Integer -> SMGen -> EFix -> IO (Set EFix)
    randomRepair' start gen fix_so_far = do
      cur_time <- getCPUTime
      let diff = cur_time - start
      -- CPUTime is in picoseconds
      if diff >= fromIntegral randSearchBudget * 1_000_000_000_000
        then do
          logStr INFO "Time budget done, returning!"
          return Set.empty
        else do
          let prog' = replaceExpr fix_so_far prog_at_ty
          hole_cands <- findEvaluatedHoles (desc ~> prog')
          let (r_hole_ind, gen') = nextInteger 0 (fromIntegral (length hole_cands - 1)) gen
              chosen_hole = hole_cands !! fromIntegral r_hole_ind
          -- We get a  list of list of fits. However, we assume that there are
          -- no other holes in the program
          [fits] <- getHoleFits compConf exprFitCands [chosen_hole] >>= processFits compConf
          let fix_cands :: [(EFix, EExpr)]
              fix_cands = map (first Map.fromList) $ replacements chosen_hole fits
              (r_fix_ind, gen'') = nextInteger 0 (fromIntegral (length fix_cands - 1)) gen'
              (chosen_fix, fixed_prog) = fix_cands !! fromIntegral r_fix_ind
              complete_fix = mergeFixes fix_so_far chosen_fix

          [check_res] <- checkFixes desc [fixed_prog]

          let keep_going = randomRepair' start gen'' complete_fix
              done =
                do
                  logStr INFO "Fix found!"
                  logOut INFO complete_fix
                  Set.insert complete_fix <$> randomRepair' start gen'' Map.empty

          case check_res of
            Right True -> done
            Right False -> keep_going
            Left res ->
              if and res
                then done
                else keep_going

    efcs = Just exprFitCands
    EProb {..} = progProblem
    prog_at_ty = progAtTy e_prog e_ty
