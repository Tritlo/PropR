{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Endemic.Search.Random.Search
-- Description : Provides an Random Search Algorithm based on typed holes.
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides a simple random search algorithm.
-- For Pseudocode see "randomRepair".
--
-- Random Search sometimes performs suprisingly well, see https://dl.acm.org/doi/abs/10.1145/2568225.2568254.
-- We use it mostly to justify the use of Genetic algorithms, as random search is a good baseline to show that genetic search actually yields benefits.
-- Nevertheless, it might be faster as it is low on ritual and for easy problems, or simply by luck, it can find solutions faster.
module Endemic.Search.Random.Search where

import Control.Arrow (first, second)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic.Configuration
import Endemic.Eval (runGhcWithCleanup)
import Endemic.Repair (checkFixes, findEvaluatedHoles, getHoleFits, processFits, repairAttempt, replacements)
import Endemic.Search.Random.Configuration (RandomConf (..))
import Endemic.Traversals (replaceExpr)
import Endemic.Types
import Endemic.Util
import GHC (runGhc)
import System.CPUTime (getCPUTime)
import System.Random.SplitMix (SMGen, mkSMGen, nextInteger)

-- | Tries to repair a program by randomly punching holes and trying random replacements.
--
-- Pseudocode:
-- While SearchBudget Left
--    pick random number N between [1,maxFixSize]
--    punch N holes in the Program
--    fill every hole with a random fix
--    if Fix-Not-Seen
--      Check fix
--      Add Fix either to Results or Seen-Fixes
--    Update Timer
--
-- The patches are growing randomly to a certain max-size, from which a new random one is created.
randomRepair :: RandomConf -> ProblemDescription -> IO (Set EFix)
randomRepair r@RandConf {..} desc@ProbDesc {..} = do
  logStr INFO "Starting random search!"
  start <- getCPUTime
  seed <- newSeed
  randomRepair' start (mkSMGen $ fromIntegral seed) Map.empty
  where
    randomRepair' ::
      -- | Currently passed time in pico-seconds
      Integer ->
      -- | A premade Split-Mix Generator, to help with randomness
      SMGen ->
      -- | Running Fix, to be changed randomly
      EFix ->
      -- | Set of found fixes
      IO (Set EFix)
    randomRepair' start gen fix_so_far = do
      logOut AUDIT fix_so_far
      cur_time <- getCPUTime
      let diff = cur_time - start
          budget_over = diff >= budgetInPicoSeconds
          fix_too_big = Map.size fix_so_far >= randMaxFixSize
      if budget_over || fix_too_big
        then
          if budget_over
            then logStr INFO "Random Search Budget exhausted, returning!" >> return Set.empty
            else do
              logStr VERBOSE "Randomly growing Fix got too big, starting new Random-Fix"
              randomRepair' start gen Map.empty
        else do
          let prog' = applyFixToEProg e_prog fix_so_far
          hole_cands <- collectStats $ findEvaluatedHoles (desc <~ prog')
          let (r_hole_ind, gen') = nextInteger 0 (fromIntegral (length hole_cands - 1)) gen
              chosen_hole = hole_cands !! fromIntegral r_hole_ind
          -- We get a  list of list of fits. However, we assume that there are
          -- no other holes in the program
          -- TODO: Where are the "<interactive>" coming from?

          runGhcWithCleanup compConf $ do
            ~[fits] <- collectStats $ getHoleFits compConf exprFitCands [first (: []) chosen_hole]
            let fix_cands' :: [(EFix, EExpr)]
                fix_cands' = map (first Map.fromList) $ replacements (snd chosen_hole) fits
                fix_cands = map (second (replicate (length e_prog))) fix_cands'
                (r_fix_ind, gen'') = nextInteger 0 (fromIntegral (length fix_cands - 1)) gen'
                (chosen_fix, fixed_prog) = fix_cands !! fromIntegral r_fix_ind
                -- We have to make sure that the chosen_fix takes precedence.
                complete_fix = mergeFixes chosen_fix fix_so_far
            -- TODO: Does this even work?
            ~[check_res] <- liftIO $ collectStats $ checkFixes desc [fixed_prog]

            let keep_going = randomRepair' start gen'' complete_fix
                done = do
                  logStr INFO "Fix found in Random Search!"
                  logOut INFO complete_fix
                  if randStopOnResults
                    then return $ Set.singleton complete_fix
                    else Set.insert complete_fix <$> randomRepair' start gen'' Map.empty
                try_again = randomRepair' start gen'' fix_so_far

            liftIO $ case check_res of
              -- We might want to avoid programs that timeout or fail for some reason.
              -- Default is to keep them in, as we may want to repair programs that don't terminate (GCD Example)
              Right False ->
                if randIgnoreFailing
                  then try_again
                  else keep_going
              res -> if isFixed res then done else keep_going

    efcs = Just exprFitCands
    EProb {..} = progProblem
    budgetInPicoSeconds = fromIntegral randSearchBudget * 1_000_000_000_000
