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
import Data.List (nub)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic.Configuration.Types (ProblemDescription (..), (<~))
import Endemic.Eval (runGhc')
import Endemic.Repair (checkFixes, repairAttempt)
import Endemic.Search.Exhaustive.Configuration (ExhaustiveConf (..))
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
  logStr INFO "Starting exhaustive Search"
  -- Note: we don't have to recompute the fixes again and again
  all_fix_combs <-
    lazyAllCombsByLevel mergeFixes <$> case initialFixes of
      Just fixes -> return fixes
      _ -> map fst <$> repairAttempt desc

  let isFixed (Right x) = x
      isFixed (Left ps) = and ps
      loop ::
        -- | The fixes to check, a (lazy) list of changes to test.
        -- A check is a bunch of changes, hence a list too. The lay list is sorted ascending in length, the 1-Change entries are in the first list of list, the 2 Change entries are in the second list of list ...
        [[EFix]] ->
        -- | Current depth of levels, just for better debugging and logging
        Int ->
        IO (Set EFix) -- The results found within a certain time-budget
      loop [] _ = return Set.empty -- Initial Case on creation, the first set of changes is the empty list. Also invoked if we exhaust the exhaustive search.
      loop ([] : lvls) n = loop lvls (n + 1) -- This case happens when we exhausted one level of changes
      loop (lvl : lvls) n = do
        -- Normal case where we have checks left in the current level
        logStr VERBOSE ("Remaining Fixes of length " ++ (show n) ++ " : " ++ show (length lvl))
        cur_time <- getCPUTime
        let diff = cur_time - start
            budget_over = diff >= budgetInPicoSeconds
        if budget_over
          then logStr INFO "Time budget done, returning!" >> return Set.empty
          else do
            let (check_list, rest) = List.splitAt exhBatchSize lvl
            logStr VERBOSE $ "  ... thereof un-cached & unseen in last batch: " ++ show (length check_list)
            mapM_ (logOut AUDIT) check_list
            fixes <-
              if null check_list
                then return Set.empty
                else
                  Set.fromList . map fst
                    . filter (isFixed . snd)
                    . zip check_list
                    <$> runGhc' compConf (checkFixes desc (map (eProgToEProgFix . applyFixToEProg e_prog) check_list))
            if Set.null fixes
              then loop (rest : lvls) n
              else do
                mapM_ (logOut INFO) $ Set.toList fixes
                if exhStopOnResults
                  then return fixes
                  else Set.union fixes <$> loop (rest : lvls) n

  loop all_fix_combs 1
  where
    EProb {..} = progProblem
    -- PicoSeconds are the granularity provided by the CPU-Time
    budgetInPicoSeconds = fromIntegral exhSearchBudget * 1_000_000_000_000

-- | Provides all unique combinations in a lazy way
-- Crucial, as otherwise all Fixes would need to be computed pre-emptively, making an iterative search nearly impossible.
--
-- "Finally, some lazy evaluation magic!"
lazyAllCombsByLevel :: Ord a => (a -> a -> a) -> [a] -> [[a]]
lazyAllCombsByLevel comb fixes = fixes : lacbl' (Set.fromList fixes) fixes fixes
  where
    lacbl' _ _ [] = []
    lacbl' seen orig cur_level = merged : lacbl' (seen `Set.union` mset) orig cur_level
      where
        merged = nub $ filter (not . (`Set.member` seen)) $ orig >>= (flip map cur_level . comb)
        mset = Set.fromList merged
