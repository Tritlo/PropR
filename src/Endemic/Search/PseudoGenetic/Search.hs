{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Endemic.Search.PseudoGen
-- Description : Holds the PseudoGenetic Programming Parts of Endemic
-- License     : MIT
-- Stability   : experimental
--
-- This module holds simple genetic operators used to find fixes in Endemic. The
-- primary building brick is an EFix (See Endemic.Types) that resembles a set of
-- changes done to the Code. The goodness of a certain fix is expressed by the
-- failing and succeeding properties, which are a list of boolean values (true
-- for passing properties, false for failing).
module Endemic.Search.PseudoGenetic.Search (pseudoGeneticRepair) where

import Control.Concurrent.Async (mapConcurrently)
import Data.Default
import Data.Function (on)
import Data.List (groupBy, sortOn, tails)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord (Down (Down))
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic.Configuration
import Endemic.Eval (getExprFitCands)
import Endemic.Repair (checkFixes, repairAttempt)
import Endemic.Search.PseudoGenetic.Configuration
import Endemic.Traversals (replaceExpr)
import Endemic.Types
import Endemic.Util
import GHC (HsExpr (HsLet), NoExtField (NoExtField))
import GhcPlugins (Outputable (ppr), isSubspanOf, noLoc, pprPanic)

-- |
--   An Individual consists of a "Fix", that is a change to be applied,
--   and a list of properties they fulfill or fail, expressed as an boolean array.
--   A perfect candidate would have an array full of true, as every property is hold.
type Individual = (EFix, [Bool])

-- |
--   This fitness is currently simply counting the hold properties.
--   fitness (_,[true,true,false,true]) = 3
fitness :: Individual -> Float
fitness = fromIntegral . length . filter id . snd

-- |
--   This method computes the estimated fitness of the offspring of two individuals.
--   WARNING: The fitness of the offspring is only estimated, not evaluated.
complementary :: PseudoGenConf -> Individual -> Individual -> Float
complementary gc a b = avg $ map fitness $ crossover gc a b

individuals :: [(EFix, TestSuiteResult)] -> [Individual]
individuals = mapMaybe fromHelpful
  where
    fromHelpful (fs, Left r) | or r = Just (fs, r)
    fromHelpful _ = Nothing

-- |
-- This method computes best pairings for individuals, and returns them in
-- descending order.  I.E. the first element of the returned list will be the pair
-- of individuals that produces the offspring with the best fitness.
-- TODO: remove self-pairing, atleast if self-mating does not affect the fix
makePairings :: PseudoGenConf -> [Individual] -> [(Individual, Individual)]
makePairings gc indivs =
  sortOn (Down . uncurry (complementary gc)) $
    [(x, y) | (x : ys) <- tails indivs, y <- ys]

-- TODO: Better heuristics
pruneGeneration :: PseudoGenConf -> [Individual] -> [Individual]
pruneGeneration PseudoGenConf {..} new_gen =
  -- genIndividuals is the number of individuals pro generation, and is provided in PseudoGenConf
  take genIndividuals $ mapMaybe isFit new_gen
  where
    avg_fitness :: Float
    avg_fitness = avg $ map fitness new_gen
    isFit ind | fitness ind >= avg_fitness * 0.75 = Just ind
    isFit _ = Nothing

-- | Checks a given fix-result for successfulness, that is passing all tests.
successful :: [(a, TestSuiteResult)] -> [(a, TestSuiteResult)]
successful = filter (isFixed . snd)

-- |
--   This method combines individuals by merging their fixes.
--   The assumed properties of the offspring are the logically-or'd properties of
--   the input individuals.
--   WARNING: The assumed property-fullfillment is a heuristic.
crossover :: PseudoGenConf -> Individual -> Individual -> [Individual]
crossover _ i1 i2 = [breed i1 i2, breed i2 i1]
  where
    -- lor = logical or
    breed :: Individual -> Individual -> Individual
    breed (f1, r1) (f2, r2) = (f1 `mergeFixes` f2, lor r1 r2)
    lor :: [Bool] -> [Bool] -> [Bool]
    lor (False : xs) (False : ys) = False : lor xs ys
    lor (_ : xs) (_ : ys) = True : lor xs ys
    lor [] [] = []
    lor _ _ = error "crossover length mismatch!"

selection :: PseudoGenConf -> [Individual] -> IO [Individual]
selection gc indivs = pure $ pruneGeneration gc de_duped
  where
    -- Compute the offsprigns of the best pairings
    pairings :: [Individual]
    pairings = concatMap (uncurry (crossover gc)) $ makePairings gc indivs
    -- Deduplicate the pairings
    de_duped = deDupOn (Map.keys . fst) pairings

pseudoGeneticRepair :: PseudoGenConf -> ProblemDescription -> IO (Set EFix)
pseudoGeneticRepair
  gc@PseudoGenConf {..}
  desc@ProbDesc
    { compConf = cc,
      repConf = rc,
      progProblem = prob@EProb {..},
      exprFitCands = efcs,
      initialFixes = mb_initial_fixes
    } = do
    first_attempt <- case mb_initial_fixes of
      Just fixes ->
        zip fixes <$> checkFixes desc (map (`replaceExpr` progAtTy e_prog e_ty) fixes)
      _ -> collectStats $ repairAttempt desc
    if not $ null $ successful first_attempt
      then return (Set.fromList $ map fst $ successful first_attempt)
      else do
        let prog_at_ty = progAtTy e_prog e_ty
            runGen (fix, _) = do
              let n_prog = replaceExpr fix prog_at_ty
              map (\(f, r) -> (f `mergeFixes` fix, r))
                <$> collectStats (repairAttempt (desc <~ n_prog))
            loop :: [(EFix, TestSuiteResult)] -> Int -> IO (Set EFix)
            loop gen n
              | not (null $ successful gen) =
                do
                  logStr INFO $ "Repair found after " ++ show n ++ " rounds!"
                  return $ Set.fromList $ map fst $ successful gen
            loop _ rounds | rounds >= genRounds = return Set.empty
            loop attempt rounds = do
              let gen = individuals attempt
              new_gen <- selection gc gen
              let ga = avg $ map fitness gen
                  nga = avg $ map fitness new_gen
              logStr INFO $ "GENERATION " ++ show rounds
              logStr INFO $ "AVERAGE FITNESS: " ++ show ga
              logStr INFO $ "NEXT GEN ESTIMATED FITNESS: " ++ show nga
              logStr INFO $ "IMPROVEMENT: " ++ show (nga - ga)
              logStr AUDIT "PREV GEN"
              mapM_ (logOut AUDIT . \g -> (fst g, fitness g)) gen
              logStr AUDIT "NEXT GEN"
              mapM_ (logOut AUDIT . \g -> (fst g, fitness g)) new_gen
              let mapGen = if genPar then mapConcurrently else mapM
              (t, new_attempt) <- collectStats $ time $ concat <$> mapGen runGen new_gen
              logStr INFO $ "ROUND TIME: " ++ showTime t
              loop new_attempt (rounds + 1)
        loop first_attempt 1

-- |
-- Computes the average value of an array of integrals.
-- It is used to compute the average fitness of a generation.
avg :: Fractional a => [a] -> a
avg as = sum as / fromIntegral (length as)

deDupOn :: (Eq b, Ord b) => (a -> b) -> [a] -> [a]
deDupOn f as = map snd $ filter ((`Set.member` grouped) . fst) zas
  where
    zas = zip [(0 :: Int) ..] as
    zbs = zip [(0 :: Int) ..] $ map f as
    grouped = Set.fromList $ map (fst . head) $ groupBy ((==) `on` snd) $ sortOn snd zbs
