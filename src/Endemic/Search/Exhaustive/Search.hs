{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Endemic.Search.Exhaustive.Search
-- Description : Provides an Exhaustive Search Algorithm based on typed holes.
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides a simple exhaustive search algorithm.
-- For Pseudocode see "exhaustiveRepair".
--
-- Exhaustive Search is expected to perform well for small programs,
-- with bigger programs it's a bit of luck whether you happen to visit the relevant parts first.
module Endemic.Search.Exhaustive.Search where

import Control.Arrow (first, (***))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic.Configuration.Types (ProblemDescription (..), (<~))
import Endemic.Eval (runGhc')
import Endemic.Repair (checkFixes, repairAttempt)
import Endemic.Search.Exhaustive.Configuration (ExhaustiveConf (..), Unmaterialized (umExhaustiveSearchBudget))
import Endemic.Traversals (replaceExpr)
import Endemic.Types
import Endemic.Util
import System.CPUTime (getCPUTime)

-- | Tries to repair a program by exhaustively replacing all elements with holes,
-- Then checking all possible replacements for the hole.
-- After all expressions have been replaced by holes and their respective hole-fits,
-- the program is similiarly tried to be fixed with two-holes-at-once.
-- This procedure is repeated until a time-budget is over.
exhaustiveRepair :: ExhaustiveConf -> ProblemDescription -> IO (Set EFix)
exhaustiveRepair r@ExhaustiveConf {..} desc@ProbDesc {..} = do
  start <- getCPUTime
  -- We use BFS, i.e. check all 1 level fixes, then all 2 level fixes, etc:
  logStr VERBOSE "Starting exhaustive search!"
  -- Note: we don't have to recompute the fixes again and again
  all_fix_combs <-
    lazyAllCombsByLevel <$> case initialFixes of
      Just fixes -> return fixes
      _ -> map fst <$> repairAttempt desc

  let isFixed (Right x) = x
      isFixed (Left ps) = and ps
      loop :: Set EFix -> [[EFix]] -> IO (Set EFix)
      loop _ [] = return Set.empty
      loop checked ([] : lvls) = loop checked lvls
      loop checked (lvl : lvls) = do
        logOut VERBOSE (length lvl)
        cur_time <- getCPUTime
        let diff = cur_time - start
            budget_over = diff >= budgetInPicoSeconds
        if budget_over
          then logStr INFO "Time budget done, returning!" >> return Set.empty
          else do
            let (to_check, rest) = List.splitAt exhBatchSize lvl
                -- Our way of constructing the combinations gives a lot of
                -- repition, so we can cache tose we've checked already
                -- to avoid having to check again.
                not_checked = Set.fromList $ filter (not . (`Set.member` checked)) to_check
                checked' = not_checked `Set.union` checked
                check_list = Set.toList not_checked
            mapM_ (logOut VERBOSE) check_list
            fixes <-
              Set.fromList . map fst
                . filter (isFixed . snd)
                . zip check_list
                <$> runGhc' compConf (checkFixes desc (map (eProgToEProgFix . applyFixToEProg e_prog) check_list))
            if Set.null fixes
              then loop checked' (rest : lvls)
              else do
                logStr INFO $ "Found fixes after " ++ show (Set.size checked') ++ " checks!"
                mapM_ (logOut INFO) $ Set.toList fixes
                if exhStopOnResults
                  then return fixes
                  else Set.union fixes <$> loop checked' (rest : lvls)

  loop Set.empty all_fix_combs
  where
    EProb {..} = progProblem
    -- PicoSeconds are the granularity provided by the CPU-Time
    budgetInPicoSeconds = fromIntegral exhSearchBudget * 1_000_000_000_000

-- | Provides all combinations of fixes, for a given level, in a lazy way.
-- Crucial, as otherwise all Fixes would need to be computed pre-emptively, making an iterative search nearly impossible.
--
-- "Finally, some lazy evaluation magic!"
lazyAllCombsByLevel :: [EFix] -> [[EFix]]
lazyAllCombsByLevel fixes = fixes : lacbl' fixes fixes
  where
    lacbl' orig cur_level = merged : lacbl' orig merged
      where
        merged = orig >>= (flip map cur_level . mergeFixes)
