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

**Genetic - Naming**
A Chromosome is made up by Genotypes, which are the building bricks of changes/diffs.
In our Context, a Genotype is a single diff we apply to the Code resembled by an EFix, and the Chromosome is a [EFix].
A Phenotype is the "physical implementation" of a Chromosome, in our context that is the program with all the patches applied.
That is, our [EFix] turns from a Genotype to a Phenotype once it is run against the properties.
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
    - We could move caching into the fitness function, if we e.g. add a "hidden" reader monad to it.
      As long as the fitness here is pure that's fine and might make some tasks much nicer
      if we can easily re-calculate fitness instead of carrying it around.
      We could re-implement it similar to StdGen which also has a hidden state within the Monad.
    - Timeouts are not actually interrupting - the are checked after a generation.
      That can lead to a heavy plus above the specified timeout, e.g. by big populations on Islands.
-}
{-# LANGUAGE FlexibleInstances #-}
module Synth.Gen2 where

import Control.Monad(when, replicateM)
import System.Random
import Data.Maybe
import Data.List(sortBy)
import Data.Time.Clock

import Data.IORef
import System.Random
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


getRandomDouble :: Double -> Double -> GenMonad Double
getRandomDouble lo hi =
     do gen <- lift get
        let (res, new_gen) = randomR (lo, hi) gen
        lift (put new_gen)
        return res
-- |
--   Merging fix-candidates is mostly applying the list of changes in order.
--   The only addressed special case is to discard the next change,
--   if the next change is also used at the same place in the second fix.
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

-- OK for GhcPs expressions
instance Eq (HsExpr GhcPs) where
    (==) = (==) `on` showSDocUnsafe .ppr

type GenMonad = R.ReaderT GeneticConfiguration (ST.StateT StdGen IO)
-- BIG TODO with Matthì:
-- instance Chromosome [Efix] where
    -- TODO: Mutation should add or remove elements from the [Efix]

pickElementUniform :: (RandomGen g) => [a] -> g -> Maybe (a,g)
pickElementUniform [] _ = Nothing
pickElementUniform xs g = let (ind, g') = uniformR (0, length xs) g
                          in Just (xs !! ind, g')


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
                    lift $ lift $ repairAttempt cc prob {e_prog = n_prog} ecfs
                 case pickElementUniform possibleFixes gen of
                     Nothing -> error "no possible fixes!!"
                     -- Fix res here is:
                     -- + Right True if all the properties are correct (perfect fitnesss!)
                     -- + Right False if the program doesn't terminate (worst fitness)..
                     --    Blacklist this fix?
                     -- + Left [Bool] if it's somewhere in between.
                     Just ((fix, fix_res), gen) ->
                         do lift (ST.put gen)
                            return (mergeFixes fix e1)


    fitness _ = 1.0
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
  , dropRate :: Double             -- ^ The probability of how often we drop during mutation
  , progProblem :: EProblem -- The problem we're trying to solve
  , compConf :: CompileConfig
  , exprFitCands :: [ExprFitCand]
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

It also optionally runs Island Evolution depending on the Configuration. See module comment on more information.
-}
geneticSearch ::
    (Chromosome g) =>
    GeneticConfiguration
    -> IO [g] -- ^ The solutions found for the problems. Empty if none are found.
geneticSearch conf
    -- Case A: We do not have an Island Configuration - we do a "normal" genetic Search with Environment Selection (Best )
    | isNothing (islandConfiguration conf) = do
    -- If no: Proceed normal Evolution
        -- TODO: Log start time, to catch time for initial Population Generation
        let seed' = seed conf
            its = iterations conf
            -- Create Initial Population
            firstPop = initialPopulation seed' (populationSize conf)
        -- TODO: Some log Info here
        results <- geneticSearch' its 0 conf firstPop
        -- TODO: Some Log Info here too
        return results

    -- Case B: We do have an Island Configuration - we go all out for the coolest algorithms
    | otherwise = do
    -- If yes: Split Iterations by MigrationInterval, create sub-configurations and run sub-genetic search per config
        --TODO: Log here time
        let seed' = seed conf
            its   = iterations conf
            iConf = fromJust $ islandConfiguration conf
            populations :: Chromosome g => [[g]]
            populations = [(initialPopulation seed' (populationSize conf)) | _ <- [1 .. (islands iConf)]]
        --TODO: Log here time
        results <- islandSearch its 0 conf iConf populations
        --TODO: Log here time again!
        return results
        -- Careful: Timer is max timer of all Island Timers?
    -- Case C:

    where
        -- | Recursive Step of Genetic Search without Islands, based on environmental selection (best fit elements survive, every element is tested)
        geneticSearch' ::  (Chromosome g) =>
            Int         -- ^ The remaining iterations to perform before abort, stops on 0
            -> Int      -- ^ The current time in Ms, used to check for timeout
            -> GeneticConfiguration
            -> [g]      -- ^ The (current) population on which to perform search on
            -> IO [g]   -- ^ The results found, for which the fitness function is correct. Collected over all generations, ordered by generations ascending
        -- Case A: Iterations Done, return empty results
        geneticSearch' 0 _ _ _ = return []
        -- Case B: Iterations left, check on other abortion criteria
        geneticSearch' n currentTime conf pop =
            if currentTime > (maxTimeInMS conf)
            then return []
            else do
                start <- getCurrentTime
                let
                    -- Select the right mechanism according to Configuration (tournament vs. Environment)
                    selectionMechanism =
                        if (isJust $ tournamentConfiguration conf)
                        then tournamentSelectedGeneration
                        else environmentSelectedGeneration
                    nextGen = selectionMechanism conf pop
                end <- getCurrentTime
                let
                    -- Determine Winners
                    winners = [species | (fitness,species) <- nextGen, fitness == 0]
                    nextPop = [x | (_,x) <- nextGen]
                    -- Calculate passed time in ms
                    timediff :: Int
                    timediff = round $ (diffUTCTime end start) * 1000
                -- End Early when any result is ok
                -- when (not (null winners) && stopOnResults conf) (return winners)
                -- Run Genetic Search with New Pop,updated Timer, GenConf & Iterations - 1
                recursiveResults <- geneticSearch' (n-1) (currentTime + timediff) conf nextPop
                return (winners ++ recursiveResults)

        -- | recursive step of genetic search with Islands.
        --
        islandSearch ::  (Chromosome g) =>
            Int         -- ^ The remaining iterations to perform before abort, stops on 0
            -> Int      -- ^ The current time in Ms, used to check for timeout
            -> GeneticConfiguration
            -> IslandConfiguration -- ^ separated out from normal Conf, to not always extract from Maybe.
            -> [[g]]      -- ^ The (current) populations, separated by island, on which to perform search on
            -> IO [g]   -- ^ The results found, for which the fitness function is correct. Collected over all generations and all Islands, ordered by generations ascending
        -- Case A: Iterations Done, return empty results
        islandSearch 0 _ _ _ _ = return []
        -- Case B: We have Iterations Left
        islandSearch n currentTime conf iConf populations =
            -- Check for Timeout
            if currentTime > (maxTimeInMS conf)
            then return []
            else do
                start <- getCurrentTime
                let
                    -- Select the right mechanism according to Configuration (tournament vs. Environment)
                    selectionMechanism =
                        if (isJust $ tournamentConfiguration conf)
                        then tournamentSelectedGeneration
                        else environmentSelectedGeneration
                    nextGens = map (selectionMechanism conf) populations
                end <- getCurrentTime
                let
                    -- Determine Winners
                    winners = concat [[species | (f,species) <- nextGen , f == 0] | nextGen <- nextGens]
                    -- We calculate the passed generations by substracting current remaining its from total its
                    passedIterations = (iterations conf) - n
                    -- We check whether we have a migration, by using modulo on the passed generations
                    nextPops = if (mod passedIterations (migrationInterval iConf)) == 0
                        then [map (snd) gen | gen <- (migrate (seed conf) iConf nextGens)]
                        else [map (snd) gen | gen <- nextGens]
                    -- Calculate passed time in ms
                    timediff :: Int
                    timediff = round $ (diffUTCTime end start) * 1000
                -- End Early when any result is ok
                -- when (not (null winners) && stopOnResults conf) (return winners)
                -- Run Genetic Search with New Pop,updated Timer, GenConf & Iterations - 1
                recursiveResults <- islandSearch (n-1) (currentTime + timediff) conf iConf nextPops
                return (winners ++ recursiveResults)

        -- | Process a single generation of the GA, without filtering or checking for any times.
        -- We pass the fitness out with the next generation, to save us some computations upstream
        environmentSelectedGeneration :: (Chromosome g) => GeneticConfiguration -> [g] -> [(Double,g)]
        environmentSelectedGeneration conf existingPopulation =
            let
                -- extract seed, to be shorter
                seed' = seed conf
                -- Partition into Pairs
                parents = partitionInPairs seed' existingPopulation
                -- Seggs
                children = [crossover seed' x | x <- parents]
                children' = [a | (a,b) <- children] ++ [b | (a,b) <- children]
                -- For every new baby, coinFlip whether to mutate, mutate if true
                mutatedChildren = [if coin (seed conf) (mutationRate conf) then mutate seed' x else x | x <- children']
                -- Merge Parents & Offspring into an intermediate-population of size 2*N
                mergedPopulation = mutatedChildren ++ existingPopulation
                -- calculate fitness
                fitnessedPopulation = [(fitness x, x) | x <- mergedPopulation]
                -- select best fitting N elements, we assume 0 (smaller) fitness is better
            in take (populationSize conf) $ sortBy (\(f1,_) (f2,_) -> compare f1 f2) fitnessedPopulation
        tournamentSelectedGeneration :: (Chromosome g) => GeneticConfiguration -> [g] -> [(Double,g)]
        tournamentSelectedGeneration conf pop =
            let
                seed' = seed conf
                tConf =  fromJust (tournamentConfiguration conf)
                champions = catMaybes [pickByTournament seed' (size tConf) (rounds tConf) pop | _ <- [1 .. (populationSize conf)]]
                parents = partitionInPairs seed' champions
                children = [crossover seed' x | x <- parents]
                children' = [a | (a,b) <- children] ++ [b | (a,b) <- children]
                -- For every new baby, coinFlip whether to mutate, mutate if true
                mutatedChildren = [if coin (seed conf) (mutationRate conf) then mutate seed' x else x | x <- children']
                -- Unlike Environment Selection, in Tournament the "Elitism" is done passively in the Tournament
                -- The Parents are not merged and selected later, they are just discarded
                -- In concept, well fit parents will make it through the tournament twice, keeping their genes anyway.
                newPopulation = mutatedChildren
                -- calculate fitness
                fitnessedPopulation = [(fitness x, x) | x <- newPopulation]
            in fitnessedPopulation
        -- | Performs the migration from all islands to all islands,
        -- According to the IslandConfiguration provided.
        -- It always migrates, check whether migration is needed/done is done upstream.
        migrate :: Chromosome g =>
            RandomNumberProvider   -- ^ Required for random island migrations
            -> IslandConfiguration -- ^ The Configuration by which to perform the migration
            -> [[(Double,g)]]      -- ^ The populations in which the migration will take place
            -> [[(Double,g)]]      -- ^ The populations after migration, the very best species of every island are duplicated to the receiving island (intended behaviour)
        migrate seed iConf islandPops =
            let
                -- Select the best M species per island
                migrators = [take (migrationSize iConf) $ sortBy (\(f1,_) (f2,_) -> compare f1 f2) pop | pop <- islandPops]
                -- Drop the worst M species per Island
                receivers = [drop (migrationSize iConf) $ sortBy (\(f1,_) (f2,_) -> compare f1 f2) pop | pop <- islandPops]
                -- Rearrange the migrating species either by moving one clockwise, or by shuffling them
                migrators' = if (ringwiseMigration iConf)
                             then tail migrators ++ [head migrators]
                             else shuffle seed migrators
                pairs = zip receivers migrators'
            in map (\(a,b) -> a ++ b) pairs

-- TODO: Add Reasoning when to use Tournaments, and suggested params
pickByTournament :: Chromosome g => RandomNumberProvider -> Int -> Int -> [g] -> Maybe g
-- Case A: No Elements in the Pop
pickByTournament _ _ _ [] =     Nothing
-- Case B: One Element in the Pop - Shortwire to it
pickByTournament _ _ _ [a] =    Just a
--- Case C: Actual Tournament Selection taking place, including Fitness Function
pickByTournament seed tournamentSize rounds population =
    fmap (\(a,b)-> b) $ pickByTournament' seed tournamentSize rounds population Nothing
    where
        -- TODO: Explain that I carry the fitness around to save computations. Maybe remove this with cached fitness
        pickByTournament' :: (Chromosome g) =>
            RandomNumberProvider
            -> Int          -- ^ Tournment size, how many elements the champion is compared to per round
            -> Int          -- ^ (Remaining) Tournament Rounds
            -> [g]          -- ^ Population from which to draw from
            -> Maybe (Double,g)  -- ^ Current Champion, Nothing if iteration started or on missbehaviour
            -> Maybe (Double,g)   -- ^ Champion after the selection, Nothing on Empty Populations
        pickByTournament' _ _ 0 _ (Just champion) = Just champion
        pickByTournament' _ _ 0 _ (Nothing) = Nothing
        pickByTournament' seed tournamentSize 1 population Nothing =
            fittest $ catMaybes [(pickRandomElement seed population) | _ <- [1..tournamentSize]]
        pickByTournament' seed tournamentSize 1 population (Just (a,b)) =
            let challengers = catMaybes [(pickRandomElement seed population) | _ <- [1..tournamentSize]]
            in fittest (b:challengers)
        pickByTournament' seed tournamentSize n population champ =
                    let
                        challengers = catMaybes [(pickRandomElement seed population) | _ <- [1..tournamentSize]]
                        recursiveBest = pickByTournament' seed tournamentSize (n-1) population champ
                        fighters = maybeToList (fmap snd champ) ++ maybeToList (fmap snd recursiveBest) ++ challengers
                    in fittest fighters

-- | For a given list of cromosomes, applies the fitness function and returns the
-- very fittest (head of the sorted list) if the list is non-empty.
fittest :: (Chromosome g) => [g] -> Maybe (Double,g)
fittest gs = listToMaybe $ sortBy (\(f1,_) (f2,_) -> compare f1 f2) $ map (\x -> (fitness x, x)) gs

-- ===============================================================
-- "Non Genetic" Helpers
-- ===============================================================

-- | Determines whether an even with chance x happens.
-- A random number between 0 and 1 is created and compared to x,
-- if the drawn number is smaller it returns true, false otherwise.
-- This leads e.g. that (coin 0.5) returns true and false in 50:50
-- While (coin 0.25) returns true and false in 25:75 ratio
coin ::
    RandomNumberProvider        -- ^ A seed to control the randomness
    -> Double                   -- ^ The Probabilty of passing, between 0 (never) and 1 (always).
    -> Bool                     -- ^ Whether or not the event occured
coin _ 0 = False -- Shortcut for false, no random used
coin _ 1 = True -- Shortcut for true, no random used
coin gen th = undefined

-- | This method finds pairs from a given List.
-- It is used for either finding partners to crossover,
-- Or in terms of Island Evolution to find Islands that swap Individuals.
partitionInPairs :: (Eq a) => RandomNumberProvider -> [a] -> [(a,a)]
partitionInPairs _ [] = []
partitionInPairs _ [a] = []
partitionInPairs seed as =
    let pair = pickRandomPair seed as
    in
        if isNothing pair
        then []
        else (fromJust pair) : (partitionInPairs seed (removePairFromList as (fromJust pair)))

-- | Returns the same list shuffled.
shuffle :: RandomNumberProvider -> [a] -> [a]
shuffle = undefined

-- | Picks a random pair of a given List.
-- The pair is not removed from the List.
-- Must be given a List with an even Number of Elements.
pickRandomPair :: RandomNumberProvider -> [a] -> Maybe (a,a)
pickRandomPair seed [] = Nothing -- ^ not allowed!
pickRandomPair seed [a] = Nothing -- ^ not allowed!
pickRandomPair seed as = if even (length as)
    then undefined
    else Nothing

pickRandomElement :: RandomNumberProvider -> [a] -> Maybe a
pickRandomElement seed gs = undefined

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
