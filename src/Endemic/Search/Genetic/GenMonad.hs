{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Endemic.Search.Genetic.GenMonad where

import Control.Arrow (second)
import Control.Concurrent.Async (mapConcurrently)
import Control.DeepSeq (NFData (..))
import Control.Monad (replicateM)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as ST
import Data.Function (on)
import Data.List (partition, sortBy, sortOn)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic.Configuration
import Endemic.Eval (runGhc')
import Endemic.Repair (checkFixes, repairAttempt)
import Endemic.Search.Genetic.Configuration
import Endemic.Search.Genetic.Types
import Endemic.Search.Genetic.Utils
import Endemic.Traversals (replaceExpr)
import Endemic.Types (EFix, EProblem (..), TestSuiteResult)
import Endemic.Util (applyFixToEProg, collectStats, eProgToEProgFix, mergeFixes, mergeFixes', progAtTy)
import GHC (GhcPs, HsExpr, SrcSpan, isSubspanOf)
import GhcPlugins (Outputable (..), liftIO, ppr, showSDocUnsafe)
import System.Random

-- ===========                                    ==============
-- ===             EFix Chromosome implementation            ===
-- ===========                                    ==============

-- | This instance is required to implement "EQ" for Efixes. and it's associated GenMonad
-- The Efixes are a Map SrcSpan (HsExpr GhcPs), where the SrcSpan (location in the program) already has a suitable
-- EQ instance. For our purposes, it is hence fine to just compare the "toString" of both Expressions.
-- We do not recommend using this as an implementation for other programs.
splitGenList :: StdGen -> [StdGen]
splitGenList g = g' : splitGenList g''
  where
    (g', g'') = System.Random.split g

instance Chromosome EFix where
  crossover (f1, f2) = collectStats $ efixCrossover f1 f2

  mutate e1 = head <$> mutateMany [e1]
  mutateMany exprs = collectStats $ do
    desc@ProbDesc {..} <- liftDesc R.ask
    GConf {..} <- liftConf R.ask
    gen <- getGen
    flips <- mapM (\e -> (e,) <$> tossCoin dropRate) exprs
    let (to_drop_w_inds, to_mutate_w_inds) = partition (snd . snd) $ zip [0 :: Int ..] flips
        (to_mutate_inds, to_mutate) = map fst <$> unzip to_mutate_w_inds
        (to_drop_inds, to_drop) = map fst <$> unzip to_drop_w_inds
        drop' :: EFix -> StdGen -> EFix
        drop' e _ | Map.null e = e
        drop' e g =
          let Just (key_to_drop, _) = pickElementUniform (Map.keys e) g
           in Map.delete key_to_drop e
        (gen' : gens) = splitGenList gen
        dropped = zipWith drop' to_drop gens

        EProb {..} = progProblem
        n_progs = map (applyFixToEProg e_prog) to_mutate
        (gen'' : gens') = splitGenList gen'
        selection ((p, pFixes), gen) = do
          -- To avoid taking only single steps, we take between
          -- 1 and maxFixSizeSteps at a time.
          let (fix_size, gen') = uniformR (1, maxFixSizeStep) gen
          case pickRandomElements fix_size gen' pFixes of
            -- No possible fix, meaning we don't have any locations
            -- to change... which means we've already solved it!
            -- TODO: is this always the case when we get Nothing
            -- here?
            ([], _) -> return p
            -- Fix res here is:
            -- + Right True if all the properties are correct (perfect fitnesss!)
            -- + Right False if the program doesn't terminate (worst fitness)..
            --    Blacklist this fix?
            -- + Left [Bool] if it's somewhere in between.
            -- We special case for the
            ([(fix, mb_fix_res)], _) -> do
              let mf = mergeFixes fix p
              case mb_fix_res of
                Just fix_res -> updateCache (mf, basicFitness mf fix_res)
                _ -> return ()
              return mf
            -- We just merge the fixes straight up.
            -- TODO: Similarly to the problem with precomputingFixes, it is
            -- not neccessarily correct to just merge them in case the types
            -- change. However, for non-refinement holes this should be fine.
            (fixes, _) -> return $ foldl mergeFixes p $ map fst fixes
        mapGen = if useParallelMap then mapConcurrently else mapM

    -- TODO: Is it safe to use the initial_fixes here? Should be, since fixes
    -- will only ever make types MORE specific, meaning that subsequent
    -- replacements will either be OK or cause a compilation error (which then)
    -- receives the lowest possible fitness.
    possibleFixes <- case initialFixes of
      Just fixes -> return $ replicate (length n_progs) $ zip fixes (repeat Nothing)
      _ ->
        collectStats $
          liftIO $ mapGen (fmap (map (second Just)) . repairAttempt . setProg desc) n_progs
    mutated <- mapM selection (zip (zip to_mutate possibleFixes) gens')
    putGen gen''
    return $ map snd $ sortOn fst $ zip to_mutate_inds mutated ++ zip to_drop_inds dropped

  fitness e1 = head <$> fitnessMany [e1]
  fitnessMany exprs = collectStats $ do
    fc <- getCache
    let lookups :: [(EFix, Maybe Double)]
        lookups = map (\e -> (e,) $ fc Map.!? e) exprs
        (done_w_inds, to_compute_w_inds) = partition (isJust . snd . snd) $ zip [0 :: Int ..] lookups
        (to_compute_inds, to_compute) = map fst <$> unzip to_compute_w_inds
        (done_inds, done) = map (\(d, Just r) -> (d, r)) <$> unzip done_w_inds
    if null to_compute
      then return $ map snd done
      else do
        res <- zipWith (\e f -> (e, basicFitness e f)) to_compute
                 <$> unsafeComputePopResults to_compute
        putCache (Map.fromList res `Map.union` fc)
        return $
          map (snd . snd) $
            sortOn fst $
              zip to_compute_inds res ++ zip done_inds done

  unsafeComputePopResults to_compute = do
    desc@ProbDesc {..} <- liftDesc R.ask
    GConf {..} <- liftConf R.ask
    let EProb {..} = progProblem
        n_progs = map (applyFixToEProg e_prog) to_compute
    liftIO (checkFixes desc $ map eProgToEProgFix n_progs)

  initialPopulation 0 = return [] -- We don't want to do any work if there's no work to do.
  initialPopulation n = collectStats $
    do
      desc@ProbDesc {..} <- liftDesc R.ask
      GConf {..} <- liftConf R.ask
      possibleFixes <- case initialFixes of
        Just fixes -> return $ zip fixes (repeat Nothing)
        _ -> liftIO $ map (second Just) <$> repairAttempt desc

      replicateM n $ do
        gen <- getGen
        case pickElementUniform possibleFixes gen of
          -- No fixes are possible, so we go away empty handed.
          Nothing -> return Map.empty
          -- Fix res here is:
          -- + Right True if all the properties are correct (perfect fitnesss!)
          -- + Right False if the program doesn't terminate (worst fitness)..
          --    Blacklist this fix?
          -- + Left [Bool] if it's somewhere in between.
          Just ((fix, mb_fix_res), gen''') -> do
            case mb_fix_res of
              Just fix_res -> updateCache (fix, basicFitness fix fix_res)
              _ -> return ()
            putGen gen'''
            return fix

-- | Calculates the fitness of an EFix by checking it's FixResults (=The Results of the property-tests).
-- It is intended to be cached using the Fitness Cache in the GenMonad.
basicFitness :: EFix -> TestSuiteResult -> Double
basicFitness mf fix_res =
  case fix_res of
    Left bools -> 1 - fitness_func (mf, bools) / fromIntegral (length bools)
    Right True -> 0 -- Perfect fitness
    Right False -> 1 -- The worst
  where
    fitness_func = fromIntegral . length . filter id . snd

-- | A bit more sophisticated crossover for efixes.
-- The Efixes are transformed to a list and for each chromosome a crossover point is selected.
-- Then the Efixes are re-combined by their genes according to the selected crossover point.
efixCrossover :: EFix -> EFix -> GenMonad (EFix, EFix)
efixCrossover f_a f_b = do
  gen <- getGen
  let (aGenotypes, bGenotypes) = (Map.toList f_a, Map.toList f_b)
      (crossedAs, crossedBs, gen') = crossoverLists gen aGenotypes bGenotypes
  putGen gen'
  return (Map.fromList crossedAs, Map.fromList crossedBs)
  where
    crossoverLists ::
      (RandomGen g) =>
      g ->
      [(SrcSpan, HsExpr GhcPs)] ->
      [(SrcSpan, HsExpr GhcPs)] ->
      ([(SrcSpan, HsExpr GhcPs)], [(SrcSpan, HsExpr GhcPs)], g)
    -- For empty chromosomes, there is no crossover possible
    crossoverLists gen [] [] = ([], [], gen)
    crossoverLists gen as bs =
      let (crossoverPointA, gen') = uniformR (0, length as) gen
          (crossoverPointB, gen'') = uniformR (0, length bs) gen'
          (part1A, part2A) = splitAt crossoverPointA as
          (part1B, part2B) = splitAt crossoverPointB bs
       in (mergeFixes' part1A part2B, mergeFixes' part1B part2A, gen'')

-- | This method tries to reduce a Fix to a smaller, but yet still correct Fix.
-- To achieve this, the Parts of a Fix are tried to be removed and the fitness function is re-run.
-- If the reduced Fix still has a perfect fitness, it is returned in a list of potential fixes.
minimizeFix :: EFix -> GenMonad (Set EFix)
minimizeFix bigFix = do
  fitnesses <- fitnessMany candidateFixes
  let fitnessedCandidates = zip fitnesses candidateFixes
      reducedWinners = filter ((== 0) . fst) fitnessedCandidates
      reducedWinners' = Set.fromList $ map snd reducedWinners
  return reducedWinners'
  where
    candidates :: Set (Set (SrcSpan, HsExpr GhcPs))
    candidates = Set.powerSet $ Set.fromDistinctAscList $ Map.toAscList bigFix
    candidateFixes :: [EFix]
    candidateFixes = map (Map.fromDistinctAscList . Set.toAscList) $ Set.toList candidates

runGenMonad :: GeneticConfiguration -> ProblemDescription -> Int -> GenMonad a -> IO a
runGenMonad conf desc seed =
  fmap (fst . fst) . runGenMonad' conf desc (mkStdGen seed) Map.empty

-- A version of RGM that takes a generator and cache instead of a seed.
runGenMonad' ::
  GeneticConfiguration ->
  ProblemDescription ->
  StdGen ->
  FitnessCache ->
  GenMonad a ->
  IO ((a, StdGen), FitnessCache)
runGenMonad' conf desc gen fc action = do
  let withConf = R.runReaderT action conf
      withParams = R.runReaderT withConf desc
      withGen = ST.runStateT withParams gen
  ST.runStateT withGen fc

liftConf :: R.ReaderT GeneticConfiguration _ a -> GenMonad a
liftConf = id

liftDesc :: R.ReaderT ProblemDescription _ a -> GenMonad a
liftDesc = lift

liftGen :: ST.StateT StdGen _ a -> GenMonad a
liftGen = lift . lift

liftCache :: ST.StateT FitnessCache _ a -> GenMonad a
liftCache = lift . lift . lift

-- Some getters and setters for the monad
getGen :: GenMonad StdGen
getGen = liftGen ST.get

putGen :: StdGen -> GenMonad ()
putGen = liftGen . ST.put

getCache :: GenMonad FitnessCache
getCache = liftCache ST.get

putCache :: FitnessCache -> GenMonad ()
putCache = liftCache . ST.put

updateCache :: (EFix, Double) -> GenMonad ()
updateCache (k, v) = getCache >>= putCache . Map.insert k v

-- | Helper to clearer use "randomR" of the RandomPackage for our GenMonad.
getRandomDouble ::
  -- | lower bound, included in the possible values
  Double ->
  -- | upper bound, included in the possible values
  Double ->
  -- | a values chosen from a uniform distribution (low,high), and the updated GenMonad with the new Generator
  GenMonad Double
getRandomDouble lo hi =
  do
    gen <- getGen
    let (res, new_gen) = randomR (lo, hi) gen
    putGen new_gen
    return res

tossCoin :: Double -> GenMonad Bool
tossCoin rate = do
  gen <- getGen
  let (res, gen') = coin rate gen
  putGen gen'
  return res
