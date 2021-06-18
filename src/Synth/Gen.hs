{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Synth.Gen
-- Description : Holds the Genetic Programming Parts of HenProg
-- License     : MIT
-- Stability   : experimental
--
-- This module holds simple genetic operators used to find fixes in HenProg. The
-- primary building brick is an EFix (See Synth.Types) that resembles a set of
-- changes done to the Code. The goodness of a certain fix is expressed by the
-- failing and succeeding properties, which are a list of boolean values (true
-- for passing properties, false for failing).
module Synth.Gen where

import Control.Concurrent.Async
import Data.Function (on)
import Data.List (groupBy, nub, sortOn, tails)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord
import qualified Data.Set as Set
import GHC
import GhcPlugins (isSubspanOf, noLoc)
import Synth.Eval
import Synth.Repair
import Synth.Traversals
import Synth.Types
import Synth.Util
import System.Random.MWC

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
complementary :: GenConf -> Individual -> Individual -> Float
complementary gc a b = avg $ map fitness $ crossover gc a b

individuals :: [(EFix, Either [Bool] Bool)] -> [Individual]
individuals = mapMaybe fromHelpful
  where
    fromHelpful (fs, Left r) | or r = Just (fs, r)
    fromHelpful _ = Nothing

-- |
-- This method computes best pairings for individuals, and returns them in
-- descending order.  I.E. the first element of the returned list will be the pair
-- of individuals that produces the offspring with the best fitness.
-- TODO: remove self-pairing, atleast if self-mating does not affect the fix
makePairings :: GenConf -> [Individual] -> [(Individual, Individual)]
makePairings gc indivs =
  sortOn (Down . uncurry (complementary gc)) $
    [(x, y) | (x : ys) <- tails indivs, y <- ys]

-- TODO: Better heuristics
pruneGeneration :: GenConf -> [Individual] -> [Individual]
pruneGeneration GenConf {..} new_gen =
  -- genIndividuals is the number of individuals pro generation, and is provided in GenConf
  take genIndividuals $ mapMaybe isFit new_gen
  where
    avg_fitness :: Float
    avg_fitness = avg $ map fitness new_gen
    isFit ind | fitness ind >= avg_fitness * 0.75 = Just ind
    isFit _ = Nothing

-- | Checks a given fix-result for successfulness, that is passing all tests.
successful :: Eq b => [(a, Either b Bool)] -> [(a, Either b Bool)]
successful = filter (\(_, r) -> r == Right True)

-- |
--   This method combines individuals by merging their fixes.
--   The assumed properties of the offspring are the logically-or'd properties of
--   the input individuals.
--   WARNING: The assumed property-fullfillment is a heuristic.
crossover :: GenConf -> Individual -> Individual -> [Individual]
crossover _ i1 i2 = [breed i1 i2, breed i2 i1]
  where
    -- lor = logical or
    breed :: Individual -> Individual -> Individual
    breed i1@(f1, r1) i2@(f2, r2) = (f1 `mergeFixes` f2, lor r1 r2)
    lor :: [Bool] -> [Bool] -> [Bool]
    lor (False : xs) (False : ys) = False : lor xs ys
    lor (_ : xs) (_ : ys) = True : lor xs ys
    lor [] [] = []

mutation :: GenConf -> GenIO -> [Individual] -> IO [Individual]
mutation gc rand_gen inds = do
  r <- uniformR (0, length inds -1) rand_gen
  -- TODO: Make configureable, e.g. how often, and such. This is very crude
  return [inds !! r]

selection :: GenConf -> GenIO -> [Individual] -> IO [Individual]
selection gc rand_gen indivs = do
  let new_gen = pruneGeneration gc de_duped
  mut <- mutation gc rand_gen de_duped
  -- TODO: There's something weird going on with the deduping here
  return $ deDupOn (Map.keys . fst) (new_gen ++ mut)
  where
    -- Compute the offsprigns of the best pairings
    pairings :: [Individual]
    pairings = concatMap (uncurry (crossover gc)) $ makePairings gc indivs
    -- Deduplicate the pairings
    de_duped = deDupOn (Map.keys . fst) pairings

genRepair :: CompileConfig -> EProblem -> IO [EFix]
genRepair cc@CompConf {genConf = gc@GenConf {..}} prob@EProb {..} = do
  efcs <- collectStats $ getExprFitCands cc $ noLoc $ HsLet NoExtField e_ctxt $ noLoc undefVar
  first_attempt <- collectStats $ repairAttempt cc prob (Just efcs)
  -- initialize generator
  seed <- case genSeed of
    -- TODO: This will return the same number again and again
    Just gs -> return $ toSeed gs
    _ -> do
      s <- createSystemSeed
      logStr INFO $ "Using seed " ++ show s
      return s
  rand_gen <- restore seed
  if not $ null $ successful first_attempt
    then return (map fst $ successful first_attempt)
    else do
      let prog_at_ty = progAtTy e_prog e_ty
          runGen (fix, _) = do
            let n_prog = replaceExpr fix prog_at_ty
            map (\(f, r) -> (f `mergeFixes` fix, r))
              <$> collectStats (repairAttempt cc prob {e_prog = n_prog} (Just efcs))
          loop :: [(EFix, Either [Bool] Bool)] -> Int -> IO [EFix]
          loop gen round
            | not (null $ successful gen) =
              do
                logStr INFO $ "Repair found after " ++ show round ++ " rounds!"
                return $ deDupOn Map.keys $ map fst $ successful gen
          loop _ rounds | rounds >= genRounds = return []
          loop attempt rounds = do
            let gen = individuals attempt
            new_gen <- selection gc rand_gen gen
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
deDupOn f as = map snd $ filter (\(i, a) -> i `Set.member` grouped) zas
  where
    zas = zip [(0 :: Int) ..] as
    zbs = zip [(0 :: Int) ..] $ map f as
    grouped = Set.fromList $ map (fst . head) $ groupBy ((==) `on` snd) $ sortOn snd zbs

-- | Merging fix-candidates is mostly applying the list of changes in order.
--   The only addressed special case is to discard the next change,
--   if the next change is also used at the same place in the second fix.
mergeFixes :: EFix -> EFix -> EFix
mergeFixes f1 f2 = Map.fromList $ mf' (Map.toList f1) (Map.toList f2)
  where
    mf' [] xs = xs
    mf' xs [] = xs
    mf' (x : xs) ys = x : mf' xs (filter (not . isSubspanOf (fst x) . fst) ys)
