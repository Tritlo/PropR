{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Endemic.Search.Genetic.Search where

import Control.Monad (foldM)
import qualified Control.Monad.Trans.Reader as R
import Data.List (partition, sortBy, sortOn)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock
import Endemic.Configuration (ProblemDescription (..))
import Endemic.Search.Genetic.Configuration
import Endemic.Search.Genetic.GenMonad
import Endemic.Search.Genetic.Types
import Endemic.Search.Genetic.Utils
import Endemic.Types
import Endemic.Util
import GhcPlugins (liftIO)

-- ===========                 ==============
-- ===           Genetic Search           ===
-- ===    All Parts to perform the GA     ===
-- ===========                 ==============

-- |
-- This is the primary method of this module.
-- It runs a genetic search that terminates in three cases:
--     a) x iterations done
--     b) n seconds passed
--     c) solutions found (optionally with early exit)
-- It will return an empty List in case of no found solutions.
--
-- The search consists of
--     - Generation of Initial Population
--     - Configuring / Using the right GA Algorithm
--     - Genetic Search (See methods for detail)
--     - Extraction of Results
--
-- It also optionally runs Island Evolution depending on the Configuration.
-- See module comment on more information.
geneticSearch ::
  (Chromosome g) =>
  -- | The solutions found for the problems. Empty if none are found.
  GenMonad (Set g)
geneticSearch = collectStats $ do
  conf <- R.ask
  let its = iterations conf
  start <- liftIO getCurrentTime
  results <- case islandConfiguration conf of
    -- Case A: We do not have an Island Configuration - we do a "normal" genetic Search with Environment Selection (Best )
    Nothing -> do
      -- If no: Proceed normal Evolution
      logStr' INFO ("Starting Genetic Search at " ++ show start)
      logStr'
        INFO
        ( "Running " ++ show its
            ++ " Generations with a population of "
            ++ show (populationSize conf)
        )
      -- Create Initial Population
      firstPop <- collectStats $ initialPopulation (populationSize conf)
      logStr' DEBUG "Finished creating initial population, starting search"
      -- TODO: The Minimization cannot be done here, as this is too generic (it's for Chromosomes, not for EFixes)
      geneticSearch' its 0 firstPop

    -- Case B: We do have an Island Configuration - we go all out for the coolest algorithms
    (Just iConf) -> do
      -- If yes: Split Iterations by MigrationInterval, create sub-configurations and run sub-genetic search per config
      logStr' INFO ("Starting Genetic Search with Islands at " ++ show start)
      logStr'
        INFO
        ( "Running " ++ show its
            ++ " Generations with a population of "
            ++ show (populationSize conf)
            ++ " on "
            ++ show (islands iConf)
            ++ " Islands"
        )

      populations <- collectStats $ sequence [initialPopulation (populationSize conf) | _ <- [1 .. (islands iConf)]]
      logStr' DEBUG "Finished creating initial populations, starting search"
      -- Careful: Timer is max timer of all Island Timers?
      islandSearch its 0 populations

  end <- liftIO getCurrentTime
  logStr'
    INFO
    ( "Genetic Search finished at " ++ show end ++ " with "
        ++ show (length results)
        ++ " results"
    )
  return results
  where
    geneticSearch' ::
      (Chromosome g) =>
      -- | The remaining iterations to perform before abort, stops on 0
      Int ->
      -- | The current time in Ms, used to check for timeout
      Int ->
      -- | The (current) population on which to perform search on
      [g] ->
      -- | The results found, for which the fitness function is correct. Collected over all generations, ordered by generations ascending
      -- Case A: Iterations Done, return empty results
      GenMonad (Set g)
    geneticSearch' 0 _ _ = return Set.empty
    -- Case B: Iterations left, check on other abortion criteria
    geneticSearch' n currentTime pop = do
      conf <- R.ask
      if currentTime > maxTimeInMS conf
        then do
          logStr' INFO "Time Budget used up - ending genetic search"
          return Set.empty
        else do
          start <- liftIO getCurrentTime
          let currentGen = iterations conf - n
          logStr'
            DEBUG
            ( "Starting Generation " ++ show currentGen
                ++ " at "
                ++ show start
            )
          let -- Select the right mechanism according to
              -- Configuration (tournament vs. Environment)
              selectionMechanism =
                if isJust $ tournamentConfiguration conf
                  then tournamentSelectedGeneration
                  else environmentSelectedGeneration
          nextPop <- collectStats $ selectionMechanism pop
          end <- liftIO getCurrentTime
          -- Determine Winners
          winners <- collectStats $ selectWinners 0 nextPop
          let -- Calculate passed time in ms
              timediff :: Int
              timediff = round $ diffUTCTime end start * 1000
          -- when (not (null winners) && stopOnResults conf) (return winners)
          logStr'
            INFO
            ( "Finished Generation " ++ show currentGen ++ " at "
                ++ show end
                ++ "("
                ++ show (length winners)
                ++ " Results)"
            )
          liftIO $ do
            logStr AUDIT "Current gen:"
            mapM (logOut AUDIT) pop
            logStr AUDIT "Next gen:"
            mapM (logOut AUDIT) nextPop
          -- End Early when any result is ok
          if not (null winners) && stopOnResults conf
            then return winners
            else -- Otherwise do recursive step
            do
              -- If we replace winners, we make for every winner a new element and replace it in the population
              nextPop' <-
                if replaceWinners conf
                  then
                    let reducedpop = filter (not . (`Set.member` winners)) nextPop
                     in do
                          replacers <- collectStats $ initialPopulation (length winners)
                          return (replacers ++ reducedpop)
                  else -- If we don't replace winners, just keep the population
                    return nextPop
              -- Run Genetic Search with New Pop,updated Timer, GenConf & Iterations - 1
              recursiveResults <- geneticSearch' (n -1) (currentTime + timediff) nextPop'
              return (winners `Set.union` recursiveResults)
    islandSearch ::
      (Chromosome g) =>
      -- | The remaining iterations to perform before abort, stops on 0
      Int ->
      -- | The current time in Ms, used to check for timeout
      Int ->
      -- | The (current) populations, separated by island, on which to perform search on
      [[g]] ->
      -- | The results found, for which the fitness function is "perfect"(==0). Collected over all generations and all Islands, ordered by generations ascending
      -- Case A: Iterations Done, return empty results
      GenMonad (Set g)
    islandSearch 0 _ _ = return Set.empty
    -- Case B: We have Iterations Left
    islandSearch n currentTime populations = do
      conf <- R.ask
      let iConf = fromJust $ islandConfiguration conf
      -- Check for Timeout
      if currentTime > maxTimeInMS conf
        then do
          logStr' INFO "Time Budget used up - ending genetic search"
          return Set.empty
        else do
          start <- liftIO getCurrentTime
          let currentGen = iterations conf - n
          logStr'
            DEBUG
            ( "Starting Generation " ++ show currentGen
                ++ " at "
                ++ show start
            )
          let -- Select the right mechanism according to Configuration (tournament vs. Environment)
              selectionMechanism =
                if isJust $ tournamentConfiguration conf
                  then tournamentSelectedGeneration
                  else environmentSelectedGeneration
          nextGens <- mapM selectionMechanism populations
          end <- liftIO getCurrentTime

          -- Determine Winners (where fitness == 0)

          winners <- foldM ((. selectWinners 0) . fmap . Set.union) Set.empty nextGens
          -- We calculate the passed generations by substracting current remaining its from total its
          let passedIterations = iterations conf - n
          -- We check whether we have a migration, by using modulo on the passed generations
          nextPops <-
            if mod passedIterations (migrationInterval iConf) == 0
              then migrate nextGens
              else return nextGens
          let -- Calculate passed time in ms
              timediff :: Int
              timediff = round $ diffUTCTime end start * 1000
          logStr'
            INFO
            ( "Finished Generation " ++ show currentGen
                ++ " at "
                ++ show end
                ++ "("
                ++ show (length winners)
                ++ " Results)"
            )
          -- End Early when any result is ok
          if not (null winners) && stopOnResults conf
            then return winners
            else -- Otherwise do recursive step
            do
              nextPops' <-
                if not (replaceWinners conf)
                  then -- If we don't replace winners, just keep the population

                    return nextPops
                  else -- If we replace winners, we make for every winner a new element and replace it in the population

                    let reducedPops = map (filter (not . (`Set.member` winners))) nextPops
                        -- unlike in non island search, the winners could be on some islands while not on others (obviously)
                        -- So we have to re-fill the islands individually
                        numReplacers = map ((populationSize conf -) . length) reducedPops
                     in do
                          replacers <- mapM initialPopulation numReplacers
                          return (zipWith (++) replacers reducedPops)
              -- Run Genetic Search with New Pop,updated Timer, GenConf & Iterations - 1
              recursiveResults <- islandSearch (n -1) (currentTime + timediff) nextPops'
              return (winners `Set.union` recursiveResults)
    environmentSelectedGeneration :: (Chromosome g) => [g] -> GenMonad [g]
    environmentSelectedGeneration pop = do
      conf <- R.ask
      gen <- getGen
      let -- Partition the parentGeneration into Pairs
          (parents, gen') = partitionInPairs pop gen
      -- Perform Crossover
      children <- performCrossover parents
      let children' = uncurry (++) $ unzip children
      putGen gen'
      -- For every new baby, coinFlip whether to mutate, mutate if true
      mutated_children <- performMutation children'
      let -- Merge Parents & Offspring into an intermediate-population of size 2*N
          mergedPop = pop ++ mutated_children
      -- select best fitting N elements, we assume 0 (smaller) fitness is better
      mergedPop' <- sortPopByFitness mergedPop
      let nextPop = take (populationSize conf) mergedPop'
      return nextPop

    tournamentSelectedGeneration :: (Chromosome g) => [g] -> GenMonad [g]
    tournamentSelectedGeneration pop = do
      conf <- R.ask
      gen <- getGen
      champions <- pickNByTournament (populationSize conf) pop
      let tConf = fromJust (tournamentConfiguration conf)
          (parents, gen') = partitionInPairs champions gen
      children <- performCrossover parents
      putGen gen'
      let children' = uncurry (++) $ unzip children
      -- Unlike Environment Selection, in Tournament the "Elitism" is done passively in the Tournament
      -- The Parents are not merged and selected later, they are just discarded
      -- In concept, well fit parents will make it through the tournament twice, keeping their genes anyway.
      performMutation children'
    migrate ::
      Chromosome g =>
      -- | The populations in which the migration will take place
      [[g]] ->
      -- | The populations after migration, the very best species of every island are duplicated to the receiving island (intended behaviour)
      GenMonad [[g]]
    migrate islandPops = do
      conf <- R.ask
      gen <- getGen
      let iConf = fromJust $ islandConfiguration conf
      sortedIslands <- mapM sortPopByFitness islandPops
      let -- Select the best M species per island
          migrators = [take (migrationSize iConf) pop | pop <- sortedIslands]
          -- Drop the worst M species per Island
          receivers = [drop (migrationSize iConf) pop | pop <- sortedIslands]
          -- Rearrange the migrating species either by moving one clockwise, or by shuffling them
          (migrators', gen') =
            if ringwiseMigration iConf
              then (tail migrators ++ [head migrators], gen)
              else shuffle migrators gen
          islandMigrationPairs = zip receivers migrators'
          newIslands = map (uncurry (++)) islandMigrationPairs
      putGen gen'
      return newIslands

-- | This Method performs mostly the Genetic Search, but it adds some Efix-Specific Post-Processing.
-- As we wanted to keep the genetic search nicely generic for chromosomes, some methods like minimizing Efixes where not applicable within it.
-- This is why there is a small wrapper around it.
-- TODO: Maybe change name ?
geneticSearchPlusPostprocessing :: GenMonad (Set EFix)
geneticSearchPlusPostprocessing = do
  ProbDesc {..} <- liftDesc R.ask
  GConf {..} <- liftConf R.ask
  -- Step 0: Do the normal search
  results <- geneticSearch
  -- Step 1: Minimize dedubbed Results
  if tryMinimizeFixes
    then Set.unions <$> mapM minimizeFix (Set.toList results)
    else return results

sortPopByFitness :: Chromosome g => [g] -> GenMonad [g]
sortPopByFitness gs = do
  fitnesses <- fitnessMany gs
  let fitnessedGs = zip fitnesses gs
      -- TODO: Check if this is ascending!
      sorted = sortBy (\(f1, _) (f2, _) -> compare f1 f2) fitnessedGs
      extracted = map snd sorted
  return extracted

selectWinners ::
  Chromosome g =>
  -- | Best value to compare with, winners are the ones
  -- where fitness equal to this value
  Double ->
  -- | The species that might win
  [g] ->
  -- | the Actual winners
  GenMonad (Set g)
selectWinners win gs = do
  Set.fromList . map fst . filter ((==) win . snd) . zip gs <$> fitnessMany gs

-- | Little Helper to perform tournament Selection n times
-- It hurt my head to do it in the monad
pickNByTournament :: Chromosome g => Int -> [g] -> GenMonad [g]
pickNByTournament 0 _ = return []
pickNByTournament _ [] = return []
pickNByTournament n gs = do
  champ <- pickByTournament gs
  recursiveChampions <- pickNByTournament (n -1) gs
  return (maybeToList champ ++ recursiveChampions)

-- | Helper to perform mutation on a list of Chromosomes.
-- For every element, it checks whether to mutate, and if yes it mutates.
performMutation :: Chromosome g => [g] -> GenMonad [g]
performMutation gs = do
  ProbDesc {..} <- liftDesc R.ask
  GConf {..} <- liftConf R.ask
  flips <- mapM (\g -> (g,) <$> tossCoin mutationRate) gs
  let (to_mutate_w_inds, rest_w_inds) = partition (snd . snd) $ zip [0 :: Int ..] flips
      (rest_inds, rest) = map fst <$> unzip rest_w_inds
      (tm_inds, to_mutate) = map fst <$> unzip to_mutate_w_inds
  res <- mutateMany to_mutate
  -- This changes the order, but that should be OK?
  -- return (res ++ rest)
  return $ map snd $ sortOn fst $ zip tm_inds res ++ zip rest_inds rest

-- | Helper to perform crossover on a list of paired Chromosomes.
-- At first, it performs a coinflip whether crossover is performed, based on the crossover rate.
-- If not, duplicates of the parents are returned (this is common in Genetic Algorithms).
-- These duplicates do not hurt too much, as they still are mutated.
performCrossover :: Chromosome g => [(g, g)] -> GenMonad [(g, g)]
-- Termination Step: Empty lists do not need any action
performCrossover pairs = do
  ProbDesc {..} <- liftDesc R.ask
  GConf {..} <- liftConf R.ask
  flips <- mapM (\g -> (g,) <$> tossCoin crossoverRate) pairs
  let (to_crossover_w_inds, rest_w_inds) = partition (snd . snd) $ zip [0 :: Int ..] flips
      (rest_inds, rest) = map fst <$> unzip rest_w_inds
      (tc_inds, to_crossover) = map fst <$> unzip to_crossover_w_inds
  res <- crossoverMany to_crossover
  -- This changes the order, but that should be OK?
  -- return (res ++ rest)
  return $ map snd $ sortOn fst $ zip tc_inds res ++ zip rest_inds rest

-- TODO: Add Reasoning when to use Tournaments, and suggested params
pickByTournament :: Chromosome g => [g] -> GenMonad (Maybe g)
-- Case A: No Elements in the Pop
pickByTournament [] = return Nothing
-- Case B: One Element in the Pop - Shortwire to it
pickByTournament [a] = return (Just a)
--- Case C: Actual Tournament Selection taking place, including Fitness Function
pickByTournament population =
  do
    -- Ask for Tournament Rounds m
    ProbDesc {..} <- liftDesc R.ask
    GConf {..} <- liftConf R.ask
    let (Just tConf) = tournamentConfiguration
        tournamentRounds = tRounds tConf
    -- Perform Tournament with m rounds and no initial champion
    pickByTournament' tournamentRounds population Nothing
  where
    pickByTournament' ::
      (Chromosome g) =>
      -- | (Remaining) Tournament Rounds
      Int ->
      -- | Population from which to draw from
      [g] ->
      -- | Current Champion, Nothing if search just started or on missbehaviour
      Maybe g ->
      -- | Champion after the selection, Nothing on Empty Populations
      -- Case 1: We terminated and have a champion
      GenMonad (Maybe g)
    pickByTournament' 0 _ (Just champion) = return (Just champion)
    -- Case 2: We terminated but do not have a champion
    pickByTournament' 0 _ Nothing = return Nothing
    -- Case 3: We are in the last iteration, and do not have a champion.
    -- Have n random elements compete, return best
    pickByTournament n population curChamp = do
      ProbDesc {..} <- liftDesc R.ask
      GConf {..} <- liftConf R.ask
      gen <- getGen
      let (Just tConf) = tournamentConfiguration
          tournamentSize = tSize tConf
          (tParticipants, gen') = pickRandomElements tournamentSize gen population
      putGen gen'
      if n > 1
        then do
          recursiveChampion <- pickByTournament' (n -1) population curChamp
          fittest (maybeToList recursiveChampion ++ maybeToList curChamp ++ tParticipants)
        else do
          let newChamp = case curChamp of
                Nothing -> fittest tParticipants
                (Just champ) -> fittest (champ : tParticipants)
          newChamp

-- | For a given list of cromosomes, applies the fitness function and returns the
-- very fittest (head of the sorted list) if the list is non-empty.
-- Fitness is drawn from the GenMonads Fitness Cache
fittest :: (Chromosome g) => [g] -> GenMonad (Maybe g)
fittest gs = do
  sorted <- sortPopByFitness gs
  return (listToMaybe sorted)
