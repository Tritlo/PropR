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

**Naming**
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

    - Do we want to have unique elements in populations? Do we want this as a flag? Do we want this if the Chromosomes support EQ
    - Should we hardcode to a double-fitness with specified best-value 0 ? Keeping it like this could help with Multi-Objective things (as we would have a vector), but maybe it turns out to be YAGNI 
    - We could move caching into the fitness function, if we e.g. add a "hidden" reader monad to it. As long as the fitness here is pure that's fine and might make some tasks much nicer if we can easily re-calculate fitness instead of carrying it around 
-}
module Synth.Gen2 where

import Control.Monad(when)
import System.Random
import Data.Maybe
import Data.List(sortBy)
import Data.Time.Clock

type RandomNumberProvider = StdGen   -- ^ Short Type to make Signatures a bit more readable when Types are used. Also, the seed shows clearly which parts have random elements.

class Eq g => Chromosome g where 
    -- | TODO: We could also move the crossover to the configuration 
    crossover :: RandomNumberProvider -> (g,g) -> (g,g)                 -- ^ The Crossover Function to produce a new Chromosome from two Genes, in a seeded Fashion
    mutate :: RandomNumberProvider -> g -> g                            -- ^ The Mutation Function, in a seeded Fashion. This is a mutation that always "hits", taking care of not mutating every generation is done in genetic search.
    -- | TODO: Do we want to move Fitness out, 
    -- and just require a Function (Fitness :: Chromosome g, Ord a => g -> a) in the Method Signatures? 
    -- Or do we "bloat" our signatures heavily if we always carry it around?
    -- Optionally, we can put it into the Configuration
    fitness :: g -> Double                            -- ^ A fitness function, applicable to the Chromosome. 

    -- | Returns an Initial Population of Size p
    initialPopulation ::                    
        RandomNumberProvider                            -- ^ The seed of the initial Population 
        -> Int                                          -- ^ The size of the population
        -> [g]                                          -- ^ The first population, represented as a list of genes.
    
-- BIG TODO with Matthì: 
-- instance Chromosome [Efix] where 
    -- TODO: Mutation should add or remove elements from the [Efix]

-- | The GeneticConfiguration holds all elements and Flags for the genetic search, 
-- Used to slim down Signatures and provide a central lookup-point.    
data GeneticConfiguration = GConf
  { seed :: RandomNumberProvider,   -- ^ The Seed used for the random Elements. Should be initialized centrally, e.g. upfront in the main. 
    mutationRate :: Double,         -- ^ The chance that any one element is mutated
    iterations :: Int,              -- ^ How many iterations to do max (or exit earlier, depending on "stopOnResults")
    populationSize :: Int,          -- ^ How many Chromosomes are in one Population. In case of Island Evolution, each Island will have this population.
    timeoutInMinutes :: Double,     -- ^ How long the process should run (in Minutes)
    stopOnResults :: Bool,           -- ^ Whether or not to stop at the generation that first produces positive results

    tournamentConfiguration :: Maybe TournamentConfiguration, -- ^ Nothing to not do Tournament Selection, existing Conf will use Tournament instead (See below for more info)

    islandConfiguration :: Maybe IslandConfiguration  -- ^ Nothing to disable IslandEvolution, existing Conf will run Island Evolution (See below for more Info) 
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
    migrationSize :: Int            -- ^ How many Chromosomes will make it from one Island to another 
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
        --TODO: TBD
        return []
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
                -- TODO: Make Switch here for Tournament in Case its configured
                let nextGen = environmentSelectedGeneration conf pop
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
        tournamentSelectedGeneration = undefined 

-- TODO: Add Reasoning when to use Tournaments and Pseudocode
pickByTournament :: Chromosome g => RandomNumberProvider -> Int -> Int -> [g] -> Maybe g
-- Case A: No Elements in the Pop 
pickByTournament _ _ _ [] =     Nothing
-- Case B: One Element in the Pop - Shortwire to it 
pickByTournament _ _ _ [a] =    Just a 
--- Case C: Actual Tournament Selection taking place, including Fitness Function
pickByTournament seed tournamentSize rounds population = 
    fmap (\(a,b)-> b) $ pickByTournament' seed tournamentSize rounds population Nothing
    where 
        -- TODO: Explain that I carry the fitness around to save computations
        pickByTournament' :: (Chromosome g) => 
            RandomNumberProvider 
            -> Int          -- ^ Tournment size, how many elements the champion is compared to per round
            -> Int          -- ^ (Remaining) Tournament Rounds 
            -> [g]          -- ^ Population from which to draw from 
            -> Maybe (Double,g)  -- ^ Current Champion, Just random element for first iteration
            -> Maybe (Double,g)   -- ^ Champion after the selection 
        pickByTournament' _ _ 0 _ (Just champion) = Just champion
        pickByTournament' _ _ 0 _ (Nothing) = Nothing
        pickByTournament' seed tournamentSize 1 population Nothing = 
            fittest $ catMaybes [(pickRandomElement seed population) | _ <- [1..tournamentSize]]
        
        pickByTournament' seed tournamentSize 1 population (Just (a,b)) = 
            let challengers = catMaybes [(pickRandomElement seed population) | _ <- [1..tournamentSize]]
            in fittest (b:challengers)




-- TODO: Maybify this? To handle empty lists?
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
