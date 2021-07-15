{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Endemic.Search.Genetic.Configuration where

import Data.Default
import Endemic.Types (EProblem, ExprFitCand)
import GHC.Generics

-- ===========                 ==============
-- ===      Genetic Configurations        ===
-- ===========                 ==============

-- | The GeneticConfiguration holds all elements and Flags for the genetic search,
-- Used to slim down Signatures and provide a central lookup-point.
data GeneticConfiguration = GConf
  { -- | The chance that any one element is mutated
    mutationRate :: Double,
    -- | the chance that a crossover is performed per parent pair
    crossoverRate :: Double,
    -- | How many iterations to do max (or exit earlier, depending on "stopOnResults")
    iterations :: Int,
    -- | How many Chromosomes are in one Population. In case of Island Evolution, each Island will have this population.
    populationSize :: Int,
    -- | How long the process should run (in Minutes)
    timeoutInMinutes :: Double,
    -- | Whether or not to stop at the generation that first produces positive results
    stopOnResults :: Bool,
    -- | Nothing to not do Tournament Selection, existing Conf will use Tournament instead (See below for more info)
    tournamentConfiguration :: Maybe TournamentConfiguration,
    -- | Nothing to disable IslandEvolution, existing Conf will run Island Evolution (See below for more Info)
    -- Pick better names for these? :
    islandConfiguration :: Maybe IslandConfiguration,
    -- | The probability of how often we drop during mutation
    dropRate :: Double,
    -- | Whether or not to try to minimize the successfull fixes. This step is performed after search as postprocessing and does not affect initial search runtime.
    tryMinimizeFixes :: Bool,
    -- | Whether successfull candidates will be removed from the populations, replaced with a new-full-random element.
    replaceWinners :: Bool,
    -- | Whether to use as much parallelism as possible
    useParallelMap :: Bool
  }
  deriving (Eq, Show, Read, Generic)

instance Default GeneticConfiguration where
  def = GConf {..}
    where
      mutationRate = 0.2
      crossoverRate = 0.05
      iterations = 50
      populationSize = 64
      timeoutInMinutes = 5
      stopOnResults = True
      tournamentConfiguration = Nothing
      islandConfiguration = Nothing
      -- T
      dropRate = 0.2
      tryMinimizeFixes = False -- Not implemented
      replaceWinners = True
      useParallelMap = True

mkDefaultConf ::
  -- | The Size of the Population, must be even
  Int ->
  -- | The number of generations to be run, must be 1 or higher
  Int ->
  GeneticConfiguration
mkDefaultConf pops its = def {populationSize = pops, iterations = its}

-- Holds all attributes for the tournament selection process
data TournamentConfiguration = TConf
  { -- | how many participants will be in one round of the tournament. Population should be significantly larger than tournament size!
    tSize :: Int,
    -- | how many rounds will one participant do
    tRounds :: Int
  }
  deriving (Eq, Show, Read, Generic)

instance Default TournamentConfiguration where
  -- TODO: Sensible defaults
  def = TConf 10 5

-- | Holds all attributes required to perform an Island Evolution.
-- For more Information on Island Evolution see https://neo.lcc.uma.es/Articles/WRH98.pdf
data IslandConfiguration = IConf
  { -- | How many Islands are in place, each will have a population according to
    islands :: Int,
    -- | After how many generations will there be an Island Migration
    -- This is often done until a full circle is complete, that means that you did islands * migrationInterval generations.
    -- We keep it in mind but do not enforce it.
    -- TODO: Make a note about this in Configuration Setup
    -- TODO: Make a note on using this only for big programs / search spaces, otherwise the resources are maybe not worth it.
    migrationInterval :: Int,
    -- | How many Chromosomes will make it from one Island to another
    migrationSize :: Int,
    -- | Whether the migration is done clockwise, True for ringwise, False for random pairs of migration
    ringwiseMigration :: Bool
  }
  deriving (Eq, Show, Read, Generic)

instance Default IslandConfiguration where
  -- TODO: Sensible defaults
  def = IConf 3 5 5 False
