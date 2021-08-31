{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : PropR.Search.Exhaustive.Search
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
module PropR.Search.Exhaustive.Search where

import Control.Arrow (first, (***))
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import PropR.Configuration.Types (ProblemDescription (..), (<~))
import PropR.Eval (runGhc')
import PropR.Repair (checkFixes, repairAttempt)
import PropR.Search.Exhaustive.Configuration (ExhaustiveConf (..))
import PropR.Traversals (replaceExpr)
import PropR.Types
import PropR.Util
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
    lazyAllCombsByLevel <$> case initialFixes of
      Just fixes -> return fixes
      _ -> map fst <$> repairAttempt desc

  let isFixed (Right x) = x
      isFixed (Left ps) = and ps
      loop ::
        -- | The seen fixes, to remove duplicates and cached items
        Set EFix ->
        -- | The fixes to check, a (lazy) list of changes to test.
        -- A check is a bunch of changes, hence a list too. The lay list is sorted ascending in length, the 1-Change entries are in the first list of list, the 2 Change entries are in the second list of list ...
        [[EFix]] ->
        -- | Current depth of levels, just for better debugging and logging
        Int ->
        IO (Set EFix) -- The results found within a certain time-budget
      loop _ [] _ = return Set.empty -- Initial Case on creation, the first set of changes is the empty list. Also invoked if we exhaust the exhaustive search.
      loop checked ([] : lvls) n = do
        -- This case happens when we exhausted one level of changes
        let msg =
              unwords
                [ "No fix of length <=",
                  show n,
                  "found",
                  '(' : show (Set.size checked),
                  "fixes checked).",
                  "Checking fixes of length",
                  show (n + 1) ++ "."
                ]
        logStr INFO msg
        loop checked lvls (n + 1)
      loop checked (lvl : lvls) n = do
        -- Normal case where we have checks left in the current level
        logStr VERBOSE ("Remaining Fixes of length " ++ show n ++ " : " ++ show (length lvl))
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
                rest' = dropWhile (`Set.member` checked') rest
            logStr VERBOSE ("  ... thereof un-cached & unseen in last batch: " ++ show (length check_list))
            mapM_ (logOut AUDIT) check_list
            fixes <- case check_list of
              [] -> return Set.empty
              _ ->
                do Set.fromList
                  . map fst
                  . filter (isFixed . snd)
                  . zip check_list
                  <$> checkFixes desc (map (eProgToEProgFix . applyFixToEProg e_prog) check_list)
            if Set.null fixes
              then loop checked' (rest' : lvls) n
              else do
                logStr INFO $ unwords ["Fix found in", show (Set.size checked'), "checks!"]
                mapM_ (logOut INFO) $ Set.toList fixes
                if exhStopOnResults
                  then return fixes
                  else Set.union fixes <$> loop checked' (rest' : lvls) n

  loop Set.empty all_fix_combs 1
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
    lacbl' _ [] = []
    lacbl' orig cur_level = merged : lacbl' orig merged
      where
        merged = orig >>= (flip map cur_level . mergeFixes)
