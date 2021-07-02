{-|
Module      : Synth.Gen2
Description : Holds the (revamped) Genetic Algorithm Parts of HenProg
License     : MIT
Stability   : experimental
Portability : POSIX

This module holds the (reworked) genetic Algorithm parts of the HenProg Library.
The algorithms are more advanced and sophisticated than the initial implementation.

The primary building brick is an EFix (See "Synth.Types") that resembles a set of
changes done to the Code. The goodness of a certain fix is expressed by the
failing and succeeding properties, which are a list of boolean values (true for passing properties, false for failing).

The methods often require a RandomGen for the random parts. A RandomGen is, for non-haskellers, a random number provider.
It is expressed in Haskell as an infinite list of next-random-values.
We expect that the RandomGen is generated e.g. in the Main Method from a Seed and passed here.
See: https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html

**Prefixes**

- tX is an X related to Tournament Behaviour (e.g. tSize = TournamentSize)
- iX is an X related to Island Behaviour (e.g. iConf = IslandConfiguration)
- a "pop" is short for population
- a "gen" is short for generator, a StdGen that helps to provide random elements

**GenMonad**

We happened to come accross some challenges nicely designing this library, in particular
we needed some re-occurring parts in nearly all functions.
This is why we decided to declare the "GenMonad", that holds

- the configuration
- a random number provider
- a cache for the fitness function
- IO (as the search logs and takes times)

**Genetic - Naming**
A Chromosome is made up by Genotypes, which are the building bricks of changes/diffs.
In our Context, a Genotype is a set of diffs we apply to the Code resembled by an pair of (SourceSpan,Expression),
and the Chromosome is a EFix (A map of those).
A Phenotype is the "physical implementation" of a Chromosome, in our context that is the program with all the patches applied.
That is, our EFix turns from its Genotype to a Phenotype once it is run against the properties.
The final representation of Solutions provided by "Synth.Diff" is also a (different) Phenotype.

**Island Evolution**
We also introduce a parallelized genetic algorithm called "Island Evolution".
In concept, there are n Island with a separate population. Every x generations, a few
species migrate from one Island to another.
This should help to "breed" one partial-solution per Island, with the migration helping to bring partial solutions together.
This is particularly interesting, as maybe fixes for a program need to originate from two places or multiple changes.
In the described paper, the species migrate ring-wise and the best species are being copied,
while the worst on the receiving island are simply discarded in favor of the best from the sending island.
Further Reading:
https://neo.lcc.uma.es/Articles/WRH98.pdf

**Open Questions // Points**

    - Timeouts are not actually interrupting - the are checked after a generation.
      That can lead to a heavy plus above the specified timeout, e.g. by big populations on Islands.
      Can we do this better?
    - This file is growing quite big, we could consider splitting it up in "Genetic" and "EfixGeneticImplementation" or something like that.
      Similarly, we could maybe move some of the helpers out.
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Synth.Gen2 where

import Control.Monad(when, replicateM)
import System.Random
import Data.Maybe
import Data.List(sortBy,delete, groupBy, sortOn, partition)
import Data.Time.Clock

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Synth.Types (EFix, GenConf (GenConf), EProblem (EProb, e_prog, e_ty), CompileConfig, ExprFitCand)
import GHC (SrcSpan, HsExpr, GhcPs, isSubspanOf)
import qualified Data.Map as Map
import Data.Function (on)
import GhcPlugins (HasCallStack,ppr, showSDocUnsafe, liftIO, getOrigNameCache, CompilerInfo (UnknownCC), Outputable(..))

import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as ST
import Control.Monad.Trans.Class (lift)
import Synth.Repair (repairAttempt)
import Synth.Util
import Synth.Traversals (replaceExpr)
import Synth.Eval (checkFixes)
import qualified Data.Set as Set

import Control.Monad.Par.IO (IVar, ParIO, runParIO)
import Control.Monad.Par.Combinator (parMapM)
import Control.DeepSeq (NFData(..), rwhnf)
import Control.Concurrent.Async


-- ===========                                    ==============
-- ===                  Genetic Requirements                 ===
-- === All Instances and Setups required for genetic search  ===
-- ===========                                    ==============


-- Eq is required to remove elements from lists while partitioning.
class (Eq g, Outputable g, NFData g) => Chromosome g where
    -- | TODO: We could also move the crossover to the configuration
    crossover :: (g,g) -> GenMonad (g,g) -- ^ The Crossover Function to produce a new Chromosome from two Genes. This Crossover must "always hit", taking care of whether to do crossover or not is done in genetic search.
    mutate :: g -> GenMonad g            -- ^ The Mutation Function, in a seeded Fashion. This is a mutation that always "hits", taking care of not mutating every generation is done in genetic search.

    -- | TODO: Do we want to move Fitness out,
    -- and just require a Function (Fitness :: Chromosome g, Ord a => g -> a) in the Method Signatures?
    -- Or do we "bloat" our signatures heavily if we always carry it around?
    -- Optionally, we can put it into the Configuration
    fitness :: g -> GenMonad Double                            -- ^ A fitness function, applicable to the Chromosome.


    -- | Returns an Initial Population of Size p
    initialPopulation ::
           Int                                     -- ^ The size of the population
        -> GenMonad [g]                            -- ^ The first population, represented as a list of genes.

    -- To improve performance, we allow users to overwrite these in case
    -- they can be implemented more efficiently
    fitnessMany :: [g] -> GenMonad [Double]
    fitnessMany = mapM fitness

    mutateMany :: [g] -> GenMonad [g]
    mutateMany = mapM mutate

    crossoverMany :: [(g,g)] -> GenMonad [(g,g)]
    crossoverMany = mapM crossover

-- | This FitnessCache is created to hold the known fitness values of Efixes.
-- When the fitness function is called, it performs a lookup here.
-- In it's current implementation, the Cache is updated at the mutate step, when a new EFix is created.
-- In it's current implementation, the Cache is never cleared.
type FitnessCache = Map.Map EFix Double

-- | The GenMonad resembles the environment in which we run our Genetic Search and it's parts.
-- It was introduced to reduce the load on various signatures and provide caching easier.
-- It consists (in this order) of
-- - A Read only GeneticConfiguration
-- - A Read-Write random number provider
-- - A Read-Write cache for Fitness values
-- - IO, to perform logging and Time-Tasks
-- The order of these is not particularly important, but we moved them in order of their occurrence (that is,
-- configuration is used the most, while IO and caching are used the least)
type GenMonad = R.ReaderT GeneticConfiguration (ST.StateT StdGen (ST.StateT FitnessCache IO))


runGenMonad :: GeneticConfiguration -> Int -> GenMonad a -> IO a
runGenMonad conf seed action = do
         let withConf = R.runReaderT action conf
             withGen = ST.runStateT withConf (mkStdGen seed)
         ((result,_ :: StdGen), _ :: FitnessCache) <- ST.runStateT withGen Map.empty
         return result

-- A version of RGM that takes a generator and cache instead of a seed.
runGenMonad' :: GeneticConfiguration
             -> StdGen -> FitnessCache -> GenMonad a -> IO ((a, StdGen), FitnessCache)
runGenMonad' conf gen fc action = do
         let withConf = R.runReaderT action conf
             withGen = ST.runStateT withConf gen
         ST.runStateT withGen fc

-- Some getters and setters for the monad
getGen :: GenMonad StdGen
getGen = lift ST.get

putGen :: StdGen -> GenMonad ()
putGen = lift . ST.put

getCache :: GenMonad FitnessCache
getCache = lift (lift ST.get)

putCache :: FitnessCache -> GenMonad ()
putCache = lift . lift . ST.put

updateCache :: (EFix, Double) -> GenMonad ()
updateCache (k,v) = getCache >>= putCache . Map.insert k v

-- ===========                 ==============
-- ===      Genetic Configurations        ===
-- ===========                 ==============

-- | The GeneticConfiguration holds all elements and Flags for the genetic search,
-- Used to slim down Signatures and provide a central lookup-point.
data GeneticConfiguration = GConf
  { mutationRate :: Double         -- ^ The chance that any one element is mutated
  , crossoverRate :: Double        -- ^ the chance that a crossover is performed per parent pair
  , iterations :: Int              -- ^ How many iterations to do max (or exit earlier, depending on "stopOnResults")
  , populationSize :: Int          -- ^ How many Chromosomes are in one Population. In case of Island Evolution, each Island will have this population.
  , timeoutInMinutes :: Double     -- ^ How long the process should run (in Minutes)
  , stopOnResults :: Bool          -- ^ Whether or not to stop at the generation that first produces positive results
  , tournamentConfiguration :: Maybe TournamentConfiguration -- ^ Nothing to not do Tournament Selection, existing Conf will use Tournament instead (See below for more info)
  , islandConfiguration :: Maybe IslandConfiguration -- ^ Nothing to disable IslandEvolution, existing Conf will run Island Evolution (See below for more Info)
  -- Pick better names for these? :
  , dropRate :: Double              -- ^ The probability of how often we drop during mutation
  , progProblem :: EProblem         -- ^ The problem we're trying to solve
  , compConf :: CompileConfig       -- ^ The compiler configuration, required to retrieve mutated EFixes
  , exprFitCands :: [ExprFitCand]   -- ^ The sum of all potentially replaced elements, required to retrieve mutated EFixes

  , tryMinimizeFixes :: Bool        -- ^ Whether or not to try to minimize the successfull fixes. This step is performed after search as postprocessing and does not affect initial search runtime.
  , replaceWinners :: Bool          -- ^ Whether successfull candidates will be removed from the populations, replaced with a new-full-random element.
  , useParallelMap :: Bool          -- ^ Whether to use as much parallelism as possible
  }

mkDefaultConf ::
    Int -- ^ The Size of the Population, must be even
    -> Int -- ^ The number of generations to be run, must be 1 or higher
    -> EProblem -> CompileConfig -> [ExprFitCand] -> GeneticConfiguration
mkDefaultConf pops its prob cc ecands = GConf {..}
    where mutationRate = 0.2
          crossoverRate = 0.05
          iterations = its
          populationSize = pops
          timeoutInMinutes = 5
          stopOnResults = True
          tournamentConfiguration = Nothing
          islandConfiguration = Nothing
          -- T
          dropRate = 0.2
          progProblem = prob
          compConf = cc
          exprFitCands = ecands
          tryMinimizeFixes = False  -- Not implemented
          replaceWinners = True
          useParallelMap = True



-- Holds all attributes for the tournament selection process
data TournamentConfiguration = TConf {
    size :: Int,        -- ^ how many participants will be in one round of the tournament. Population should be significantly larger than tournament size!
    rounds :: Int       -- ^ how many rounds will one participant do
}

-- | Holds all attributes required to perform an Island Evolution.
-- For more Information on Island Evolution see https://neo.lcc.uma.es/Articles/WRH98.pdf
data IslandConfiguration = IConf {
    islands :: Int,                 -- ^ How many Islands are in place, each will have a population according to
    migrationInterval :: Int ,      -- ^ After how many generations will there be an Island Migration
    -- This is often done until a full circle is complete, that means that you did islands * migrationInterval generations.
    -- We keep it in mind but do not enforce it.
    -- TODO: Make a note about this in Configuration Setup
    -- TODO: Make a note on using this only for big programs / search spaces, otherwise the resources are maybe not worth it.
    migrationSize :: Int ,           -- ^ How many Chromosomes will make it from one Island to another
    ringwiseMigration :: Bool        -- ^ Whether the migration is done clockwise, True for ringwise, False for random pairs of migration
}


-- ===========                 ==============
-- ===           Genetic Search           ===
-- ===    All Parts to perform the GA     ===
-- ===========                 ==============


{- |
This is the primary method of this module.
It runs a genetic search that terminates in three cases:
    a) x iterations done
    b) n minutes passed
    c) solutions found (optionally with early exit)
It will return an empty List in case of no found solutions.

The search consists of
    - Generation of Initial Population
    - Configuring / Using the right GA Algorithm
    - Genetic Search (See methods for detail)
    - Extraction of Results

It also optionally runs Island Evolution depending on the Configuration.
See module comment on more information.
-}
geneticSearch ::
    (Chromosome g) =>
    GenMonad [g] -- ^ The solutions found for the problems. Empty if none are found.
geneticSearch = collectStats $ do
    conf <- R.ask
    let its   = iterations conf
    start <- liftIO getCurrentTime
    results <- case islandConfiguration conf of
        -- Case A: We do not have an Island Configuration - we do a "normal" genetic Search with Environment Selection (Best )
        Nothing -> do
        -- If no: Proceed normal Evolution
            logStr' INFO ("Starting Genetic Search at "++ show start)
            logStr' INFO ("Running " ++ show its
                       ++ " Generations with a population of "
                       ++ show (populationSize conf))
            let minimize = tryMinimizeFixes conf
                -- Create Initial Population
            firstPop <- collectStats $ initialPopulation (populationSize conf)
            logStr' DEBUG "Finished creating initial population, starting search"
            -- TODO: The Minimization cannot be done here, as this is too generic (it's for Chromosomes, not for EFixes)
            geneticSearch' its 0 firstPop

        -- Case B: We do have an Island Configuration - we go all out for the coolest algorithms
        (Just iConf) -> do
        -- If yes: Split Iterations by MigrationInterval, create sub-configurations and run sub-genetic search per config
            logStr' INFO ("Starting Genetic Search with Islands at "++ show start)
            logStr' INFO ("Running " ++ show its
                       ++ " Generations with a population of "
                       ++ show (populationSize conf) ++ " on "
                       ++ show (islands iConf) ++ " Islands")

            populations <- collectStats $ sequence [initialPopulation (populationSize conf) | _ <- [1 .. (islands iConf)]]
            logStr' DEBUG "Finished creating initial populations, starting search"
            -- Careful: Timer is max timer of all Island Timers?
            islandSearch its 0 populations

    end <- liftIO getCurrentTime
    logStr' INFO ("Genetic Search finished at " ++ show end ++ " with "
                ++ show (length results) ++" results")
    return results

    where
        -- | Recursive Step of Genetic Search without Islands, based on environmental selection (best fit elements survive, every element is tested)
        geneticSearch' ::  (Chromosome g) =>
            Int         -- ^ The remaining iterations to perform before abort, stops on 0
            -> Int      -- ^ The current time in Ms, used to check for timeout
            -> [g]      -- ^ The (current) population on which to perform search on
            -> GenMonad [g]   -- ^ The results found, for which the fitness function is correct. Collected over all generations, ordered by generations ascending
        -- Case A: Iterations Done, return empty results
        geneticSearch' 0 _ _ = return []
        -- Case B: Iterations left, check on other abortion criteria
        geneticSearch' n currentTime pop = do
            conf <- R.ask
            if currentTime > maxTimeInMS conf
            then do
                logStr' INFO "Time Budget used up - ending genetic search"
                return []
            else do
                start <- liftIO getCurrentTime
                let currentGen = iterations conf - n
                logStr' DEBUG ("Starting Generation " ++ show currentGen
                            ++ " at "++ show start)
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
                logStr' INFO ("Finished Generation " ++ show currentGen ++ " at "
                           ++ show end ++ "(" ++ show (length winners)
                           ++ " Results)")
                -- End Early when any result is ok
                if not (null winners) && stopOnResults conf
                then
                    return winners
                -- Otherwise do recursive step
                else do
                    -- If we replace winners, we make for every winner a new element and replace it in the population
                    nextPop' <-
                        if replaceWinners conf
                        then let reducedpop = deleteAll winners nextPop
                            in do
                                replacers <- collectStats $ initialPopulation (length winners)
                                return (replacers ++ reducedpop)
                        -- If we don't replace winners, just keep the population
                        else return nextPop
                    -- Run Genetic Search with New Pop,updated Timer, GenConf & Iterations - 1
                    recursiveResults <- geneticSearch' (n-1) (currentTime + timediff) nextPop'
                    return (winners ++ recursiveResults)

        -- | recursive step of genetic search with Islands.
        -- Basically, it performs the same steps as normal genetic evolution but per island,
        -- And every so often a migration takes place (see "migrate" for information)
        islandSearch ::  (Chromosome g) =>
            Int         -- ^ The remaining iterations to perform before abort, stops on 0
            -> Int      -- ^ The current time in Ms, used to check for timeout
            -> [[g]]      -- ^ The (current) populations, separated by island, on which to perform search on
            -> GenMonad [g]   -- ^ The results found, for which the fitness function is "perfect"(==0). Collected over all generations and all Islands, ordered by generations ascending
        -- Case A: Iterations Done, return empty results
        islandSearch 0 _ _ = return []
        -- Case B: We have Iterations Left
        islandSearch n currentTime populations = do
            conf <- R.ask
            let iConf = fromJust $ islandConfiguration conf
            -- Check for Timeout
            if currentTime > maxTimeInMS conf
            then do
                logStr' INFO "Time Budget used up - ending genetic search"
                return []
            else do
                start <- liftIO getCurrentTime
                let currentGen = iterations conf - n
                logStr' DEBUG ("Starting Generation " ++ show currentGen
                            ++ " at "++ show start)
                let
                    -- Select the right mechanism according to Configuration (tournament vs. Environment)
                    selectionMechanism =
                        if isJust $ tournamentConfiguration conf
                        then tournamentSelectedGeneration
                        else environmentSelectedGeneration
                nextGens <- mapM selectionMechanism populations
                end <-  liftIO getCurrentTime

                    -- Determine Winners (where fitness == 0)
                winners <- mapM (selectWinners 0) nextGens
                let winners' = concat winners
                -- We calculate the passed generations by substracting current remaining its from total its
                let passedIterations = iterations conf - n
                -- We check whether we have a migration, by using modulo on the passed generations
                nextPops <- if mod passedIterations (migrationInterval iConf) == 0
                    then migrate nextGens
                    else return nextGens
                let
                    -- Calculate passed time in ms
                    timediff :: Int
                    timediff = round $ diffUTCTime end start * 1000
                logStr' INFO ("Finished Generation " ++ show currentGen
                           ++ " at "++ show end ++ "(" ++ show (length winners)
                           ++ " Results)")
                -- End Early when any result is ok
                if not (null winners) && stopOnResults conf
                then
                    return winners'
                -- Otherwise do recursive step
                else do
                    nextPops' <-
                        if not (replaceWinners conf)
                        -- If we don't replace winners, just keep the population
                        then
                            return nextPops
                        -- If we replace winners, we make for every winner a new element and replace it in the population
                        else
                            let
                                reducedPops = map (deleteAll winners') nextPops
                                -- unlike in non island search, the winners could be on some islands while not on others (obviously)
                                -- So we have to re-fill the islands individually
                                numReplacers = map ((populationSize conf -) . length) reducedPops
                            in do
                                replacers <- mapM initialPopulation numReplacers
                                return (zipWith (++) replacers reducedPops)
                    -- Run Genetic Search with New Pop,updated Timer, GenConf & Iterations - 1
                    recursiveResults <- islandSearch (n-1) (currentTime + timediff) nextPops
                    return (winners' ++ recursiveResults)

        -- | Process a single generation of the GA, without filtering or checking for any timeouts.
        -- We expect the fitness function to be cached and 'fast'.
        -- The environment selection includes 'Elitism', which means that the offspring
        -- competes with the parents and the best N fitting amongst both generations make it to the next rounds.
        environmentSelectedGeneration :: (Chromosome g) => [g] -> GenMonad [g]
        environmentSelectedGeneration pop = do
            conf <- R.ask
            gen <- getGen
            let -- Partition the parentGeneration into Pairs
                (parents,gen') = partitionInPairs pop gen
            -- Perform Crossover
            children <- performCrossover parents
            let children' = [a | (a,b) <- children] ++ [b | (a,b) <- children]
            putGen gen'
            -- For every new baby, coinFlip whether to mutate, mutate if true
            mutated_children <- performMutation children'
            let
                -- Merge Parents & Offspring into an intermediate-population of size 2*N
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
            let
                tConf =  fromJust (tournamentConfiguration conf)
                (parents,gen') = partitionInPairs champions gen
            children <- performCrossover parents
            putGen gen'
            let children' = [a | (a,b) <- children] ++ [b | (a,b) <- children]
                -- Unlike Environment Selection, in Tournament the "Elitism" is done passively in the Tournament
                -- The Parents are not merged and selected later, they are just discarded
                -- In concept, well fit parents will make it through the tournament twice, keeping their genes anyway.
            performMutation children'
        -- | Performs the migration from all islands to all islands,
        -- According to the IslandConfiguration provided.
        -- It always migrates, check whether migration is needed/done is done upstream.
        migrate :: Chromosome g =>
            [[g]]                -- ^ The populations in which the migration will take place
            -> GenMonad [[g]]       -- ^ The populations after migration, the very best species of every island are duplicated to the receiving island (intended behaviour)
        migrate islandPops = do
            conf <- R.ask
            gen <- getGen
            let iConf = fromJust $ islandConfiguration conf
            sortedIslands <- mapM sortPopByFitness islandPops
            let
                -- Select the best M species per island
                migrators = [take (migrationSize iConf) pop | pop <- sortedIslands]
                -- Drop the worst M species per Island
                receivers = [drop (migrationSize iConf) pop | pop <- sortedIslands]
                -- Rearrange the migrating species either by moving one clockwise, or by shuffling them
                (migrators',gen') = if ringwiseMigration iConf
                             then (tail migrators ++ [head migrators],gen)
                             else shuffle migrators gen
                islandMigrationPairs = zip receivers migrators'
                newIslands = map (uncurry (++)) islandMigrationPairs
            putGen gen'
            return newIslands

-- | This Method performs mostly the Genetic Search, but it adds some Efix-Specific Post-Processing.
-- As we wanted to keep the genetic search nicely generic for chromosomes, some methods like minimizing Efixes where not applicable within it.
-- This is why there is a small wrapper around it.
-- TODO: Maybe change name ?
geneticSearchPlusPostprocessing :: GenMonad [EFix]
geneticSearchPlusPostprocessing = do
    GConf{..} <- R.ask
    -- Step 0: Do the normal search
    results <- geneticSearch
    -- Step 1: Dedup Results
    -- TODO
    let results' = results
    -- Step 2: Minimize dedubbed Results
    if tryMinimizeFixes
        then concat <$> mapM minimizeFix results'
        else return results'

sortPopByFitness :: Chromosome g => [g] -> GenMonad [g]
sortPopByFitness gs = do
    fitnesses <- fitnessMany gs
    let
        fitnessedGs = zip fitnesses gs
        -- TODO: Check if this is ascending!
        sorted = sortBy (\(f1,_) (f2,_) -> compare f1 f2) fitnessedGs
        extracted = map snd sorted
    return extracted

selectWinners :: Chromosome g
              => Double -- ^ Best value to compare with, winners are the ones
                        -- where fitness equal to this value
              -> [g]    -- ^ The species that might win
              -> GenMonad [g] -- ^ the Actual winners
selectWinners win gs = do
    map fst . filter ((==) win . snd) . zip gs <$> fitnessMany gs

-- | Little Helper to perform tournament Selection n times
-- It hurt my head to do it in the monad
pickNByTournament :: Chromosome g => Int -> [g] -> GenMonad [g]
pickNByTournament 0 _ = return []
pickNByTournament _ [] = return []
pickNByTournament n gs = do
    champ <- pickByTournament gs
    recursiveChampions <- pickNByTournament (n-1) gs
    return (maybeToList champ ++ recursiveChampions)

-- | Helper to perform mutation on a list of Chromosomes.
-- For every element, it checks whether to mutate, and if yes it mutates.
performMutation :: Chromosome g => [g] -> GenMonad [g]
performMutation gs = do
    GConf{..} <- R.ask
    flips <- mapM (\g -> (g,) <$> tossCoin mutationRate) gs
    let (to_mutate_w_inds, rest_w_inds) = partition (snd . snd) $ zip [0..] flips
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
performCrossover :: Chromosome g => [(g,g)] -> GenMonad [(g,g)]
-- Termination Step: Empty lists do not need any action
performCrossover pairs = do
    GConf{..} <- R.ask
    flips <- mapM (\g -> (g,) <$> tossCoin crossoverRate) pairs
    let (to_crossover_w_inds, rest_w_inds) = partition (snd . snd) $ zip [0..] flips
        (rest_inds, rest) = map fst <$> unzip rest_w_inds
        (tc_inds, to_crossover) = map fst <$> unzip to_crossover_w_inds
    res <- crossoverMany to_crossover
    -- This changes the order, but that should be OK?
    -- return (res ++ rest)
    return $ map snd $ sortOn fst $ zip tc_inds res ++ zip rest_inds rest

tossCoin :: Double -> GenMonad Bool
tossCoin rate = do gen <- getGen
                   let (res,gen') = coin rate gen
                   putGen gen'
                   return res

-- TODO: Add Reasoning when to use Tournaments, and suggested params
pickByTournament :: Chromosome g => [g] -> GenMonad (Maybe g)
-- Case A: No Elements in the Pop
pickByTournament [] =  return Nothing
-- Case B: One Element in the Pop - Shortwire to it
pickByTournament [a] =  return (Just a)
--- Case C: Actual Tournament Selection taking place, including Fitness Function
pickByTournament population =
    do
        -- Ask for Tournament Rounds m
        GConf{..} <- R.ask
        let
            (Just tConf) = tournamentConfiguration
            tournamentRounds = rounds tConf
        -- Perform Tournament with m rounds and no initial champion
        pickByTournament' tournamentRounds population Nothing
    where
        pickByTournament' :: (Chromosome g) =>
            Int                         -- ^ (Remaining) Tournament Rounds
            -> [g]                      -- ^ Population from which to draw from
            -> Maybe g                  -- ^ Current Champion, Nothing if search just started or on missbehaviour
            -> GenMonad (Maybe g)       -- ^ Champion after the selection, Nothing on Empty Populations
        -- Case 1: We terminated and have a champion
        pickByTournament'  0 _ (Just champion) = return (Just champion)
        -- Case 2: We terminated but do not have a champion
        pickByTournament'  0 _ Nothing = return Nothing
        -- Case 3: We are in the last iteration, and do not have a champion.
        -- Have n random elements compete, return best
        pickByTournament n population curChamp = do
                gen <- getGen
                GConf{..} <- R.ask
                let (Just tConf) = tournamentConfiguration
                    tournamentSize = size tConf
                    (tParticipants,gen') = pickRandomElements tournamentSize gen population
                putGen gen'
                if n > 1
                then do recursiveChampion <-  pickByTournament' (n-1) population curChamp
                        fittest (maybeToList recursiveChampion++maybeToList curChamp ++ tParticipants)
                else do let newChamp = case curChamp of
                                         Nothing -> fittest tParticipants
                                         (Just champ) -> fittest (champ:tParticipants)
                        newChamp

-- | For a given list of cromosomes, applies the fitness function and returns the
-- very fittest (head of the sorted list) if the list is non-empty.
-- Fitness is drawn from the GenMonads Fitness Cache
fittest :: (Chromosome g) => [g] -> GenMonad (Maybe g)
fittest gs = do
    sorted <- sortPopByFitness gs
    return (listToMaybe sorted)


-- ===========                                    ==============
-- ===             EFix Chromosome implementation            ===
-- ===========                                    ==============

-- | This instance is required to implement "EQ" for Efixes.
-- The Efixes are a Map SrcSpan (HsExpr GhcPs), where the SrcSpan (location in the program) already has a suitable
-- EQ instance. For our purposes, it is hence fine to just compare the "toString" of both Expressions.
-- We do not recommend using this as an implementation for other programs.
instance Eq (HsExpr GhcPs) where
    (==) = (==) `on` showSDocUnsafe . ppr

instance Ord (HsExpr GhcPs) where
    compare = compare `on` showSDocUnsafe . ppr

instance NFData (HsExpr GhcPs) where
    rnf expr = seq (showSDocUnsafe (ppr expr)) ()

splitGenList :: StdGen -> [StdGen]
splitGenList g = g':splitGenList g''
    where (g',g'') = System.Random.split g

instance Chromosome EFix where
    crossover (f1,f2) = collectStats $ efixCrossover f1 f2

    mutate e1 = head <$> mutateMany [e1]
    mutateMany exprs = collectStats $ do
        GConf {..} <- R.ask
        gen <- getGen
        flips <- mapM (\e -> (e,) . (not (Map.null e) &&) <$> tossCoin dropRate) exprs
        let (to_drop_w_inds, to_mutate_w_inds) = partition (snd . snd) $ zip [0..] flips
            (to_mutate_inds, to_mutate) = map fst <$> unzip to_mutate_w_inds
            (to_drop_inds  , to_drop)   = map fst <$> unzip to_drop_w_inds
            drop :: EFix -> StdGen -> EFix
            drop e g = let ks = Map.keys e
                           Just (key_to_drop, _) = pickElementUniform (Map.keys e) g
                        in Map.delete key_to_drop e
            (gen':gens) = splitGenList gen
            dropped = zipWith drop to_drop gens

            EProb{..} = progProblem
            prog_at_ty = progAtTy e_prog e_ty
            cc = compConf
            prob = progProblem
            ecfs = Just exprFitCands
            n_progs = map (`replaceExpr` prog_at_ty) to_mutate
            (gen'':gens') = splitGenList gen'
            selection ((p,pFixes),gen)
                = case pickElementUniform pFixes gen of
                    -- No possible fix, meaning we don't have any locations
                    -- to change... which means we've already solved it!
                    -- TODO: is this always the case when we get Nothing
                    -- here?
                    Nothing -> return p
                    -- Fix res here is:
                    -- + Right True if all the properties are correct (perfect fitnesss!)
                    -- + Right False if the program doesn't terminate (worst fitness)..
                    --    Blacklist this fix?
                    -- + Left [Bool] if it's somewhere in between.
                    Just ((fix, fix_res), _) -> do
                       let mf = mergeFixes fix p
                       updateCache (mf, basicFitness mf fix_res)
                       return mf
            mapGen = if useParallelMap then mapConcurrently else mapM
        possibleFixes <- collectStats $
                if null n_progs then return []
                else liftIO $ mapGen (\p -> repairAttempt cc prob {e_prog = p} ecfs) n_progs
        mutated <- mapM selection (zip (zip to_mutate possibleFixes) gens')
        putGen gen''
        return $ map snd $ sortOn fst $ zip to_mutate_inds mutated ++ zip to_drop_inds dropped


    fitness e1 = head <$> fitnessMany [e1]
    fitnessMany exprs = collectStats $ do
            fc <- getCache
            let lookups :: [(EFix, Maybe Double)]
                lookups = map (\e -> (e,) $ fc Map.!? e) exprs
                (done_w_inds, to_compute_w_inds) = partition (isJust . snd . snd) $ zip [0..] lookups
                (to_compute_inds, to_compute) = map fst <$> unzip to_compute_w_inds
                (done_inds, done) = map (\(d, Just r) -> (d,r)) <$> unzip done_w_inds
            if null to_compute
            then return $ map snd done
            else do GConf{..} <- R.ask
                    let EProb{..} = progProblem
                        prog_at_ty = progAtTy e_prog e_ty
                        cc = compConf
                        n_progs = map (`replaceExpr` prog_at_ty) to_compute
                    res <- zipWith (\e f -> (e, basicFitness e f)) to_compute <$>
                                    liftIO (checkFixes cc progProblem n_progs)
                    putCache (Map.fromList res `Map.union` fc)
                    return $ map (snd . snd) $ sortOn fst $
                             zip to_compute_inds res ++ zip done_inds done


    initialPopulation 0 = return [] -- We don't want to do any work if there's no work to do.
    initialPopulation n = collectStats $
         do GConf{..} <- R.ask
            let EProb{..} = progProblem
                cc = compConf
                prob = progProblem
                ecfs = Just exprFitCands
            possibleFixes <- liftIO $ repairAttempt cc prob ecfs
            replicateM n $ do
               gen <- getGen
               case pickElementUniform possibleFixes gen of
                  Nothing -> error "WASN'T BROKEN??"
                  -- Fix res here is:
                  -- + Right True if all the properties are correct (perfect fitnesss!)
                  -- + Right False if the program doesn't terminate (worst fitness)..
                  --    Blacklist this fix?
                  -- + Left [Bool] if it's somewhere in between.
                  Just ((fix, fix_res), gen''') -> do
                     updateCache (fix, basicFitness fix fix_res)
                     putGen gen'''
                     return fix


-- | Calculates the fitness of an EFix by checking it's FixResults (=The Results of the property-tests).
-- It is intended to be cached using the Fitness Cache in the GenMonad.
basicFitness :: EFix -> Either [Bool] Bool -> Double
basicFitness mf fix_res =
     case fix_res of
            Left bools -> 1 - fitness_func (mf, bools) / fromIntegral (length bools)
            Right True -> 0 -- Perfect fitness
            Right False -> 1 -- The worst
    where fitness_func = fromIntegral . length . filter id . snd

-- | A bit more sophisticated crossover for efixes.
-- The Efixes are transformed to a list and for each chromosome a crossover point is selected.
-- Then the Efixes are re-combined by their genes according to the selected crossover point.
efixCrossover :: EFix -> EFix -> GenMonad (EFix,EFix)
efixCrossover a b = do
    GConf{..} <- R.ask
    gen <- getGen
    let
        (aGenotypes,bGenotypes) = (Map.toList a,Map.toList b)
        (crossedAs,crossedBs,gen') = crossoverLists gen aGenotypes bGenotypes
    putGen gen'
    return (Map.fromList crossedAs, Map.fromList crossedBs)
    where
        -- | Merges two lists of expressions,
        -- with the exception that it discards expressions if the position overlaps
        mf' :: [(SrcSpan, HsExpr GhcPs)] -> [(SrcSpan, HsExpr GhcPs)] -> [(SrcSpan, HsExpr GhcPs)]
        mf' [] xs = xs
        mf' xs [] = xs
        mf' (x : xs) ys = x : mf' xs (filter (not . isSubspanOf (fst x) . fst) ys)
        -- | Makes the (randomized) pairing of the given lists for later merging
        -- TODO: Doublecheck if this can create a merge from as ++ [] depending on the crossoverpoint logic,
        -- Or if it needs to be: uniformR (1, length as - 1) gen
        crossoverLists :: (RandomGen g) => g ->
            [(SrcSpan, HsExpr GhcPs)]
            -> [(SrcSpan, HsExpr GhcPs)]
            -> ([(SrcSpan, HsExpr GhcPs)],[(SrcSpan, HsExpr GhcPs)],g)
        -- For empty chromosomes, there is no crossover possible
        crossoverLists gen [] [] = ([],[],gen)
        -- For single-gene chromosomes, there is no crossover possible
        crossoverLists gen [a] [b] = ([a],[b],gen)
        crossoverLists gen as bs =
            let
             (crossoverPointA, gen') = uniformR (1, length as) gen
             (crossoverPointB, gen'') = uniformR (1, length bs) gen'
             (part1A,part2A) = splitAt crossoverPointA as
             (part1B,part2B) = splitAt crossoverPointB bs
            in (mf' part1A part2B, mf' part1B part2A,gen'')

-- | Merging fix-candidates is mostly applying the list of changes in order.
--   The only addressed special case is to discard the next change,
--   if the next change is also used at the same place in the second fix.
mergeFixes :: EFix -> EFix -> EFix
mergeFixes f1 f2 = Map.fromList $ mf' (Map.toList f1) (Map.toList f2)
  where
    mf' [] xs = xs
    mf' xs [] = xs
    mf' (x : xs) ys = x : mf' xs (filter (not . isSubspanOf (fst x) . fst) ys)

-- | This method tries to reduce a Fix to a smaller, but yet still correct Fix.
-- To achieve this, the Parts of a Fix are tried to be removed and the fitness function is re-run.
-- If the reduced Fix still has a perfect fitness, it is returned in a list of potential fixes.
-- The output list is sorted by length of the fixes, the head is the smallest found fix.
minimizeFix :: EFix -> GenMonad [EFix]
minimizeFix bigFix = do
    fitnesses <- fitnessMany candidateFixes
    let
        fitnessedCandidates = zip fitnesses candidateFixes
        reducedWinners = filter (\(f,c)->f==0) fitnessedCandidates
        reducedWinners' = map snd reducedWinners
    return reducedWinners'
    where
        candidates = powerset $ Map.toList bigFix
        candidates' = sortBy (\c1 c2-> compare (length c1) (length c2)) candidates
        -- TODO: If I do fitness, are they still ... sorted?
        candidateFixes = map Map.fromList candidates'

-- ===========                 ==============
-- ===      "Non Genetic" Helpers         ===
-- ===========                 ==============

-- | Reads the timeOutInMinutes of a configuration and rounds it to the nearest ms
maxTimeInMS :: GeneticConfiguration -> Int
maxTimeInMS conf = round $ 1000 * 60 * timeoutInMinutes conf

-- | removes a given pair from a List, e.g.
-- > removePairFromList [2,7,12,5,1] (1,2)
-- > [7,12,5]
-- > removePairFromList [1,2,3,4,5] (5,6)
-- > [1,2,3,4]
-- Used to remove a drafted set from parents from the population for further drafting pairs.
removePairFromList :: (Eq a) => [a] -> (a,a) -> [a]
removePairFromList as (x,y) = [a | a <- as, a /= x, a /= y]

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs


-- | The normal LogSTR is in IO () and cannot be easily used in GenMonad
-- So this is a wrapper to ease the usage given that the GenMonad is completely local
-- in this module.
logStr' :: HasCallStack => LogLevel -> String -> GenMonad ()
logStr' level str = liftIO $ logStr level str

-- | Deletes a list of elements from another list.
-- > deletaAll [1,2] [1,2,3,4,3,2,1]
-- > [3,4,3]
deleteAll :: Eq a =>
    [a]         -- ^ the elements to be removed
    -> [a]      -- ^ the list of the elements to be removed
    -> [a]
deleteAll as bs = foldl (flip delete) bs as

-- ===========                 ==============
-- ===           Random Parts             ===
-- ===========                 ==============


-- | Determines whether an even with chance x happens.
-- A random number between 0 and 1 is created and compared to x,
-- if the drawn number is smaller it returns true, false otherwise.
-- This leads e.g. that (coin 0.5) returns true and false in 50:50
-- While (coin 0.25) returns true and false in 25:75 ratio
coin ::
    (RandomGen g) =>
    Double                   -- ^ The Probabilty of passing, between 0 (never) and 1 (always).
    -> g                        -- ^ The Random number provider
    -> (Bool,g)                     -- ^ Whether or not the event occured
coin 0 gen = (False,gen) -- Shortcut for false, no random used
coin 1 gen = (True,gen) -- Shortcut for true, no random used
coin th gen =
    let (val, gen') = randomR (0,1) gen
    in  (val<th,gen')

-- | This method finds pairs from a given List.
-- It is used for either finding partners to crossover,
-- Or in terms of Island Evolution to find Islands that swap Individuals.
partitionInPairs :: (Eq a, RandomGen g) => [a] -> g -> ([(a,a)],g)
partitionInPairs [] g = ([],g)
partitionInPairs [a] g = ([],g)
partitionInPairs as g =
    let nextPair = pickRandomPair as g
    in case nextPair of
        Nothing -> ([],g)
        Just (pair,g') -> let
                            reducedList = removePairFromList as pair
                            (as',g'') = partitionInPairs reducedList g'
                          in (pair:as',g'')

-- | Returns the same list shuffled.
shuffle :: (RandomGen g, Eq a) => [a] -> g -> ([a],g)
shuffle [] g = ([],g)
shuffle as g = let
                Just (a,g') = pickElementUniform as g
                as' = delete a as
                (as'',g'') = shuffle as' g'
                in (a:as'',g'')

-- | Picks n random elements from u, can give duplicates (intentional behavior)
pickRandomElements :: (RandomGen g,Eq a) => Int -> g -> [a] -> ([a],g)
pickRandomElement 0 g _ = ([],g)
pickRandomElements _ g [] = ([],g)
pickRandomElements n g as =
    let
        (asShuffled,g') = shuffle as g
        (recursiveResults,g'') = pickRandomElements (n-1) g' as
        x = head asShuffled
    in (x:recursiveResults,g'')

-- | Helper to clearer use "randomR" of the RandomPackage for our GenMonad.
getRandomDouble ::
    Double                  -- ^ lower bound, included in the possible values
    -> Double               -- ^ upper bound, included in the possible values
    -> GenMonad Double      -- ^ a values chosen from a uniform distribution (low,high), and the updated GenMonad with the new Generator
getRandomDouble lo hi =
     do gen <- getGen
        let (res, new_gen) = randomR (lo, hi) gen
        putGen new_gen
        return res

-- | Picks a random pair of a given List.
-- The pair is not removed from the List.
-- Must be given a List with an even Number of Elements.
pickRandomPair :: (Eq a, RandomGen g) => [a] -> g -> Maybe ((a,a),g)
pickRandomPair [] _ = Nothing     -- not supported!
pickRandomPair [a] _ = Nothing    -- not supported!
pickRandomPair as g = if even (length as)
    then
        let
            -- We only get justs, because we have taken care of empty lists beforehand
            Just (elem1,g') = pickElementUniform as g
            as' = delete elem1 as
            Just (elem2,g'') = pickElementUniform as' g'
        in Just ((elem1,elem2),g'')
    else Nothing

-- | Picks a random element from a list, given the list has elements.
-- All elements have the same likeliness to be drawn.
-- Returns Just (element,updatedStdGen) for lists with elements, or Nothing otherwise
pickElementUniform :: (RandomGen g) => [a] -> g -> Maybe (a,g)
pickElementUniform [] _ = Nothing
pickElementUniform xs g = let (ind, g') = uniformR (0, length xs - 1) g
                          in Just (xs !! ind, g')

