{-# LANGUAGE RecordWildCards #-}

module Synth.Gen where

import Data.Function (on)
import Data.List (groupBy, nub, sortOn, tails)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Ord
import qualified Data.Set as Set
import GhcPlugins (isSubspanOf)
import Synth.Eval
import Synth.Repair
import Synth.Replace
import Synth.Types
import Synth.Util

type Individual = (EFix, [Bool])

breed :: Individual -> Individual -> Individual
breed i1@(f1, r1) i2@(f2, r2) =
  ( if fitness i1 >= fitness i2
      then f1 `mergeFixes` f2
      else f2 `mergeFixes` f1,
    lor r1 r2
  )
  where
    lor :: [Bool] -> [Bool] -> [Bool]
    lor (False : xs) (False : ys) = False : lor xs ys
    lor (_ : xs) (_ : ys) = True : lor xs ys
    lor [] [] = []

mergeFixes :: EFix -> EFix -> EFix
mergeFixes f1 f2 = Map.fromList $ mf' (Map.toList f1) (Map.toList f2)
  where
    mf' [] xs = xs
    mf' xs [] = xs
    mf' (x : xs) ys = x : mf' xs (filter (not . isSubspanOf (fst x) . fst) ys)

fitness :: Individual -> Float
fitness = fromIntegral . length . filter id . snd

complementary :: Individual -> Individual -> Float
complementary a b = fitness $ breed a b

individuals :: [(EFix, Either [Bool] Bool)] -> [Individual]
individuals = mapMaybe fromHelpful
  where
    fromHelpful (fs, Left r) | or r = Just (fs, r)
    fromHelpful _ = Nothing

makePairings :: [Individual] -> [(Individual, Individual)]
makePairings indivs =
  sortOn (Down . uncurry complementary) $
    [(x, y) | (x : ys) <- tails indivs, y <- ys]

-- TODO: Better heuristics
pruneGeneration :: GenConf -> [Individual] -> [Individual]
pruneGeneration GenConf {..} new_gen =
  take genIndividuals $ mapMaybe isFit new_gen
  where
    avg_fitness :: Float
    avg_fitness = avg $ map fitness new_gen
    isFit ind | fitness ind >= avg_fitness * 0.75 = Just ind
    isFit _ = Nothing

successful :: Eq b => [(a, Either b Bool)] -> [(a, Either b Bool)]
successful = filter (\(_, r) -> r == Right True)

selection :: GenConf -> [Individual] -> [Individual]
selection gc indivs = pruneGeneration gc de_duped
  where
    de_duped = deDupOn (Map.keys . fst) pairings
    pairings = map (uncurry breed) $ makePairings indivs

genRepair :: CompileConfig -> EProblem -> IO [EFix]
genRepair cc@CompConf {genConf = gc@GenConf {..}} prob@EProb {..} = do
  first_attempt <- repairAttempt cc prob
  if not $ null $ successful first_attempt
    then return (map fst $ successful first_attempt)
    else do
      let prog_at_ty = progAtTy e_prog e_ty
          runGen fix = do
            let n_prog = replaceExpr fix prog_at_ty
            map (\(f, r) -> (f `mergeFixes` fix, r))
              <$> repairAttempt cc prob {e_prog = n_prog}
          loop :: [(EFix, Either [Bool] Bool)] -> Int -> IO [EFix]
          loop gen _
            | not (null $ successful gen) =
              return $ deDupOn Map.keys $ map fst $ successful gen
          loop _ rounds | rounds >= genRounds = return []
          loop attempt rounds = do
            let gen = individuals attempt
                new_gen = selection gc gen
                ga = avg $ map fitness gen
                nga = avg $ map fitness new_gen
            prDebug ""
            prDebug $ "GENERATION " ++ show rounds
            prDebug $ "AVERAGE FITNESS: " ++ show nga
            prDebug $ "IMPROVEMENT " ++ show (nga - ga)
            new_attempt <- concat <$> mapM (runGen . fst) new_gen
            loop new_attempt (rounds + 1)
      loop first_attempt 1

avg :: Fractional a => [a] -> a
avg as = sum as / fromIntegral (length as)

deDupOn :: (Eq b, Ord b) => (a -> b) -> [a] -> [a]
deDupOn f as = map snd $ filter (\(i, a) -> i `Set.member` grouped) zas
  where
    zas = zip [(0 :: Int) ..] as
    zbs = zip [(0 :: Int) ..] $ map f as
    grouped = Set.fromList $ map (fst . head) $ groupBy ((==) `on` snd) $ sortOn snd zbs