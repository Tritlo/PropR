{-# LANGUAGE RecordWildCards #-}

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

The methods often require a StdGen for the random parts. A StdGen is, for non-haskellers, a random number provider.
It is expressed in Haskell as an infinite list of next-random-values.
We expect that the StdGen is generated e.g. in the Main Method from a Seed and passed here.
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

    - Do we want to have unique elements in populations? Do we want this as a flag?
    - Timeouts are not actually interrupting - the are checked after a generation.
      That can lead to a heavy plus above the specified timeout, e.g. by big populations on Islands.
      Can we do this better?
    - we NEED the un-cached fitness, otherwise we run into issues by dop-mutate and crossover!
-}
{-# LANGUAGE FlexibleInstances #-}
module Synth.Gen2 where

import Control.Monad(when, replicateM)
import System.Random
import Data.Maybe
import Data.List(sortBy,delete)
import Data.Time.Clock

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Synth.Types (EFix, GenConf (GenConf), EProblem (EProb, e_prog, e_ty), CompileConfig, ExprFitCand)
import GHC (SrcSpan, HsExpr, GhcPs, isSubspanOf)
import qualified Data.Map as Map
import GhcPlugins (ppr, showSDocUnsafe)
import Data.Function (on)

import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as ST
import Control.Monad.Trans.Class (lift)
import Synth.Repair (repairAttempt)
import Synth.Util (progAtTy)
import Synth.Traversals (replaceExpr)

type RandomNumberProvider = StdGen   -- ^ Short Type to make Signatures a bit more readable when Types are used. Also, the seed shows clearly which parts have random elements.

-- | Merging fix-candidates is mostly applying the list of changes in order.
--   The only addressed special case is to discard the next change,
--   if the next change is also used at the same place in the second fix.
-- TODO: We want to use random parts here, where we randomly pick a crossover point to make the babies
mergeFixes :: EFix -> EFix -> EFix
mergeFixes f1 f2 = Map.fromList $ mf' (Map.toList f1) (Map.toList f2)
  where
    mf' [] xs = xs
    mf' xs [] = xs
    mf' (x : xs) ys = x : mf' xs (filter (not . isSubspanOf (fst x) . fst) ys)


-- Eq is required to remove elements from lists while partitioning.
class Eq g => Chromosome g where
    -- | TODO: We could also move the crossover to the configuration
    crossover :: (g,g) -> GenMonad (g,g) -- ^ The Crossover Function to produce a new Chromosome from two Genes, in a seeded Fashion
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

-- | This instance is required to implement "EQ" for Efixes. 
-- The Efixes are a Map SrcSpan (HsExpr GhcPs), where the SrcSpan (location in the program) already has a suitable 
-- EQ instance. For our purposes, it is hence fine to just compare the "toString" of both Expressions.
-- We do not recommend using this as an implementation for other programs.
instance Eq (HsExpr GhcPs) where
    (==) = (==) `on` showSDocUnsafe .ppr

-- | This FitnessCache is created to hold the known fitness values of Efixes. 
-- When the fitness function is called, it performs a lookup here. 
-- In it's current implementation, the Cache is updated at the mutate step, when a new EFix is created. 
-- In it's current implementation, the Cache is never cleared. 
type FitnessCache = [(EFix, Double)]

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


instance Chromosome EFix where
    -- TODO: add actual crossover point using the random, and make sure that
    -- merge fixes is actually order dependent.
    crossover (f1,f2) = return (mergeFixes f1 f2, mergeFixes f2 f1)
    mutate e1 =
      do gen <- lift ST.get
         GConf{..} <- R.ask
         let (should_drop, gen) = random gen
         if should_drop < dropRate && not (Map.null e1)
         then do let ks :: [SrcSpan]
                     ks = Map.keys e1
                     Just (key_to_drop, gen) = pickElementUniform ks gen
                 lift (ST.put gen)
                 return $ Map.delete key_to_drop e1
         else do let EProb{..} = progProblem
                     prog_at_ty = progAtTy e_prog e_ty
                     n_prog = replaceExpr e1 prog_at_ty
                     cc = compConf
                     prob = progProblem
                     ecfs = Just exprFitCands
                 possibleFixes <-
                    lift $ lift $ lift $ repairAttempt cc prob {e_prog = n_prog} ecfs
                 case pickElementUniform possibleFixes gen of
                     Nothing -> error "no possible fixes!!"
                     -- Fix res here is:
                     -- + Right True if all the properties are correct (perfect fitnesss!)
                     -- + Right False if the program doesn't terminate (worst fitness)..
                     --    Blacklist this fix?
                     -- + Left [Bool] if it's somewhere in between.
                     Just ((fix, fix_res), gen) ->
                         do lift (ST.put gen)
                            fc <- lift (lift ST.get)
                            let mf = mergeFixes fix e1
                            when (mf `notElem` map fst fc) $ do
                                    let fitness_func = fromIntegral . length . filter id . snd
                                    let nf = case fix_res of
                                                Left bools -> 1 - (fitness_func (mf, bools) / fromIntegral (length bools))
                                                Right True -> 0 -- Perfect fitness
                                                Right False -> 1 -- The worst
                                         -- here we should compute the fitness
                                    lift (lift (ST.put (nf:fc)))
                            return mf


    fitness e1 = do fc <- lift (lift ST.get)
                    let res = lookup e1 fc
                    case res of
                        Nothing -> error "Fitness not found"
                        Just r -> return r

    initialPopulation n = replicateM n (mutate Map.empty)


-- | The GeneticConfiguration holds all elements and Flags for the genetic search,
-- Used to slim down Signatures and provide a central lookup-point.
data GeneticConfiguration = GConf
  { seed :: RandomNumberProvider   -- ^ The Seed used for the random Elements. Should be initialized centrally, e.g. upfront in the main.
  , mutationRate :: Double         -- ^ The chance that any one element is mutated
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
  }

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
    - TODO: Logging

It also optionally runs Island Evolution depending on the Configuration. 
See module comment on more information.
-}
geneticSearch ::
    (Chromosome g) =>
    GenMonad [g] -- ^ The solutions found for the problems. Empty if none are found.
geneticSearch = do 
    conf <- R.ask
    case islandConfiguration conf of 
        -- Case A: We do not have an Island Configuration - we do a "normal" genetic Search with Environment Selection (Best )
        Nothing -> do
        -- If no: Proceed normal Evolution
            -- TODO: Log start time, to catch time for initial Population Generation
            let its = iterations conf
                -- Create Initial Population
            firstPop <- initialPopulation (populationSize conf)
            -- TODO: Some log Info here
            results <- geneticSearch' its 0 firstPop
            -- TODO: Some Log Info here too
            return results

        -- Case B: We do have an Island Configuration - we go all out for the coolest algorithms
        (Just iConf) -> do
        -- If yes: Split Iterations by MigrationInterval, create sub-configurations and run sub-genetic search per config
            --TODO: Log here time
            let seed' = seed conf
                its   = iterations conf
                
            populations <- sequence $ [initialPopulation (populationSize conf) | _ <- [1 .. (islands iConf)]]
            --TODO: Log here time
            results <- islandSearch its 0 populations
            --TODO: Log here time again!
            return results
            -- Careful: Timer is max timer of all Island Timers?

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
            then return []
            else do
                start <- lift $ lift $ lift $ getCurrentTime
                let
                    -- Select the right mechanism according to Configuration (tournament vs. Environment)
                    selectionMechanism =
                        if isJust $ tournamentConfiguration conf
                        then tournamentSelectedGeneration
                        else environmentSelectedGeneration
                nextPop <- selectionMechanism pop
                end <- lift $ lift $ lift $ getCurrentTime
                    -- Determine Winners
                winners <- selectWinners 0 nextPop
                let -- Calculate passed time in ms
                    timediff :: Int
                    timediff = round $ diffUTCTime end start * 1000
                -- End Early when any result is ok
                -- when (not (null winners) && stopOnResults conf) (return winners)
                -- Run Genetic Search with New Pop,updated Timer, GenConf & Iterations - 1
                recursiveResults <- geneticSearch' (n-1) (currentTime + timediff) nextPop
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
            then return []
            else do
                start <- lift $ lift $ lift $ getCurrentTime
                let
                    -- Select the right mechanism according to Configuration (tournament vs. Environment)
                    selectionMechanism =
                        if isJust $ tournamentConfiguration conf
                        then tournamentSelectedGeneration
                        else environmentSelectedGeneration
                nextGens <- sequence $ fmap selectionMechanism populations
                end <-  lift $ lift $ lift $ getCurrentTime
                
                    -- Determine Winners (where fitness == 0)
                winners <- sequence $ fmap (selectWinners 0) nextGens
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
                -- End Early when any result is ok
                -- when (not (null winners) && stopOnResults conf) (return winners)
                -- Run Genetic Search with New Pop,updated Timer, GenConf & Iterations - 1
                recursiveResults <- islandSearch (n-1) (currentTime + timediff) nextPops
                return (winners' ++ recursiveResults)

        -- | Process a single generation of the GA, without filtering or checking for any timeouts.
        -- We expect the fitness function to be cached and 'fast'. 
        -- The environment selection includes 'Elitism', which means that the offspring 
        -- competes with the parents and the best N fitting amongst both generations make it to the next rounds.
        environmentSelectedGeneration :: (Chromosome g) => [g] -> GenMonad[g]
        environmentSelectedGeneration pop = do 
            conf <- R.ask
            gen <- lift $ ST.get
            let
                -- Partition the parentGeneration into Pairs
                (parents,gen') = partitionInPairs pop gen
            -- Perform Crossover
            children <- sequence $ [crossover x | x <- parents]
            let children' = [a | (a,b) <- children] ++ [b | (a,b) <- children]
            lift $ ST.put gen'
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
            gen <- lift $ ST.get
            champions <- pickNByTournament (populationSize conf) pop
            let
                tConf =  fromJust (tournamentConfiguration conf)
                (parents,gen') = partitionInPairs champions gen
            children <- sequence $ [crossover x | x <- parents]
            lift $ ST.put gen'
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
            gen <- lift $ ST.get 
            let iConf = fromJust $ islandConfiguration conf 
            sortedIslands <- sequence (map sortPopByFitness islandPops)
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
            lift $ ST.put gen'
            return newIslands

sortPopByFitness :: Chromosome g => [g] -> GenMonad [g]
sortPopByFitness gs = do 
    fitnesses <- mapM (\x-> fitness x) gs
    let 
        fitnessedGs = zip fitnesses gs 
        -- TODO: Check if this is ascending!
        sorted = sortBy (\(f1,_) (f2,_) -> compare f1 f2) fitnessedGs
        extracted = map snd sorted
    return extracted

selectWinners :: Chromosome g => 
    Double -> -- ^ Best value to compare with, winners are the ones where fitness equal to this value
    [g] -> -- ^ The species that might win 
    GenMonad [g] -- ^ the Actual winners
selectWinners _ [] = return []
selectWinners win (g:gs) = do
    f <- fitness g
    if f == win 
    then do 
        recursiveWinners <- selectWinners win gs 
        return (g:recursiveWinners)
    else 
        selectWinners win gs

-- | Little Helper to perform tournament Selection n times 
-- It hurt my head to do it in the monad 
pickNByTournament :: Chromosome g => Int -> [g] -> GenMonad [g]
pickNByTournament 0 _ = return []
pickNByTournament _ [] = return []
pickNByTournament n gs = do 
    champ <- pickByTournament gs
    recursiveChampions <- pickNByTournament (n-1) gs
    return ((maybeToList champ) ++ recursiveChampions)

-- | Helper to perform mutation on a list of Chromosomes. 
-- For every element, it checks whether to mutate, and if yes it mutates.
-- It hurt my head to do in the monad 
performMutation :: Chromosome g => [g] -> GenMonad [g]
performMutation [] = return []
performMutation (g:gs) = do 
    GConf{..} <- R.ask
    gen <- lift $ ST.get
    let (doMutate,gen') = coin mutationRate gen
    lift $ ST.put gen' -- This must be this early, as mutate needs StdGen too
    doneElement <- if doMutate 
        then (mutate g)
        else return g
    recursiveMutated <- performMutation gs
    return (doneElement:recursiveMutated) 

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
        -- TODO: Explain that I carry the fitness around to save computations. Maybe remove this with cached fitness
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
        pickByTournament' 1 population Nothing = 
            do 
                gen <- lift $ ST.get
                GConf{..} <- R.ask
                let 
                    (Just tConf) = tournamentConfiguration
                    tournamentSize = size tConf
                    (tParticipants,gen') = pickRandomElements tournamentSize gen population
                    champion = fittest tParticipants
                lift $ ST.put gen'
                champion
        -- Case 4: We are in the last iteration, and have a champion
        -- Pick n random elements to compete with the Champion, return best
        pickByTournament' 1 population (Just currentChampion) = 
            do
                gen <- lift $ ST.get
                GConf{..} <- R.ask
                let 
                    (Just tConf) = tournamentConfiguration
                    tournamentSize = size tConf
                    (tParticipants,gen') = pickRandomElements tournamentSize gen population
                    champion = fittest (currentChampion:tParticipants)
                lift $ ST.put gen'
                champion
        -- Case 5: "normal" recursive Case 
        -- At iteration i ask for the champion of i-1 
        -- Let the current Champion, n random elements and champion from i-1 compete
        -- Return best
        pickByTournament' n population champ =
            do
                gen <- lift $ ST.get
                GConf{..} <- R.ask
                let 
                    (Just tConf) = tournamentConfiguration
                    tournamentSize = size tConf
                    (tParticipants,gen') = pickRandomElements tournamentSize gen population
                recursiveChampion <-  pickByTournament' (n-1) population champ
                lift $ ST.put gen'
                fittest ((maybeToList recursiveChampion)++(maybeToList champ) ++ tParticipants)
                
-- | For a given list of cromosomes, applies the fitness function and returns the
-- very fittest (head of the sorted list) if the list is non-empty.
-- Fitness is drawn from the GenMonads Fitness Cache
fittest :: (Chromosome g) => [g] -> GenMonad (Maybe g)
fittest gs = do 
    sorted <- sortPopByFitness gs
    return (listToMaybe sorted)

-- ===============================================================
-- "Non Genetic" Helpers
-- ===============================================================


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
     do gen <- lift ST.get
        let (res, new_gen) = randomR (lo, hi) gen
        lift (ST.put new_gen)
        return res

-- | Picks a random pair of a given List.
-- The pair is not removed from the List.
-- Must be given a List with an even Number of Elements.
pickRandomPair :: (Eq a, RandomGen g) => [a] -> g -> Maybe ((a,a),g)
pickRandomPair [] _ = Nothing     -- ^ not supported!
pickRandomPair [a] _ = Nothing    -- ^ not supported!
pickRandomPair as g = if even (length as)
    then 
        let  
            -- We only get justs, because we have taken care of empty lists beforehand
            Just (elem1,g') = pickElementUniform as g
            as' = delete elem1 as
            Just (elem2,g'') = pickElementUniform as' g'
        in Just $ ((elem1,elem2),g)
    else Nothing

-- | Picks a random element from a list, given the list has elements.
-- All elements have the same likeliness to be drawn. 
-- Returns Just (element,updatedStdGen) for lists with elements, or Nothing otherwise
pickElementUniform :: (RandomGen g) => [a] -> g -> Maybe (a,g)
pickElementUniform [] _ = Nothing
pickElementUniform xs g = let (ind, g') = uniformR (0, length xs) g
                          in Just (xs !! ind, g')
