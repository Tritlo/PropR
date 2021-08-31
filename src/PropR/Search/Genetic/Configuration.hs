{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module PropR.Search.Genetic.Configuration where

import Data.Default
import Data.Maybe (fromMaybe)
import Deriving.Aeson
import PropR.Configuration.Materializeable
import PropR.Types (EProblem, ExprFitCand)
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
    timeoutInSeconds :: Double,
    -- | Whether or not to stop at the generation that first produces positive results
    stopOnResults :: Bool,
    -- | How many of the environment-selected elements will be from the best fit. Value between 0 and 1.
    -- Default 100% Elitism Selection will be performed (legacy-behaviour).
    -- Remaining non-elite elements for the next generation are chosen at random.
    -- This value is completely omitted in case of Tournament-Selection.
    elitismRate :: Double, 
    -- | Nothing to not do Tournament Selection, existing Conf will use Tournament instead (See below for more info)
    tournamentConfiguration :: Maybe TournamentConfiguration,
    -- | Nothing to disable IslandEvolution, existing Conf will run Island Evolution (See below for more Info)
    -- Pick better names for these? :
    islandConfiguration :: Maybe IslandConfiguration,
    -- | The probability of how often we drop during mutation
    dropRate :: Double,
    -- | How big the maximum fix size step should be
    maxFixSizeStep :: Int,
    -- | Whether or not to try to minimize the successfull fixes. This step is performed after search as postprocessing and does not affect initial search runtime.
    tryMinimizeFixes :: Bool,
    -- | Whether successfull candidates will be removed from the populations, replaced with a new-full-random element.
    replaceWinners :: Bool,
    -- | Whether to use as much parallelism as possible
    useParallelMap :: Bool
  }
  deriving (Eq, Show, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[CamelToSnake]] GeneticConfiguration

instance Default GeneticConfiguration where
  def = GConf {..}
    where
      mutationRate = 0.2
      crossoverRate = 0.4
      iterations = 50
      populationSize = 64
      timeoutInSeconds = 5 * 60
      stopOnResults = True
      elitismRate = 1.0
      tournamentConfiguration = Nothing
      islandConfiguration = Nothing
      -- T
      dropRate = 0.05
      maxFixSizeStep = 1
      tryMinimizeFixes = False -- Not implemented
      replaceWinners = True
      useParallelMap = True

-- | All parameters that are passed to the genetic configuration.
-- All Elements are Maybes, if a Nothing is found we pick the defaults.
instance Materializeable GeneticConfiguration where
  data Unmaterialized GeneticConfiguration = UmGeneticConfiguration
    { umMutationRate :: Maybe Double,
      umCrossoverRate :: Maybe Double,
      umIterations :: Maybe Int,
      umPopulationSize :: Maybe Int,
      umTimeoutInSeconds :: Maybe Double,
      umStopOnResults :: Maybe Bool,
      umElitismRate :: Maybe Double,
      umTournamentConfiguration :: Maybe (Unmaterialized TournamentConfiguration),
      umIslandConfiguration :: Maybe (Unmaterialized IslandConfiguration),
      umDropRate :: Maybe Double,
      umMaxFixSizeStep :: Maybe Int,
      umTryMinimizeFixes :: Maybe Bool,
      umReplaceWinners :: Maybe Bool,
      umUseParallelMap :: Maybe Bool
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON
            '[ OmitNothingFields,
               RejectUnknownFields,
               FieldLabelModifier '[StripPrefix "um", CamelToSnake]
             ]
            (Unmaterialized GeneticConfiguration)

  conjure = UmGeneticConfiguration n n n n n n n n n n n n n n
    where
      n = Nothing

  override conf Nothing = conf
  override GConf {..} (Just UmGeneticConfiguration {..}) =
    GConf
      { mutationRate = fromMaybe mutationRate umMutationRate,
        crossoverRate = fromMaybe crossoverRate umCrossoverRate,
        iterations = fromMaybe iterations umIterations,
        populationSize = fromMaybe populationSize umPopulationSize,
        timeoutInSeconds = fromMaybe timeoutInSeconds umTimeoutInSeconds,
        stopOnResults = fromMaybe stopOnResults umStopOnResults,
        dropRate = fromMaybe dropRate umDropRate,
        maxFixSizeStep = fromMaybe maxFixSizeStep umMaxFixSizeStep,
        tryMinimizeFixes = fromMaybe tryMinimizeFixes umTryMinimizeFixes,
        replaceWinners = fromMaybe replaceWinners umReplaceWinners,
        useParallelMap = fromMaybe useParallelMap umUseParallelMap,
        elitismRate = fromMaybe elitismRate umElitismRate,
        tournamentConfiguration = overrideIfPresent tournamentConfiguration umTournamentConfiguration,
        islandConfiguration = overrideIfPresent islandConfiguration umIslandConfiguration
      }

mkDefaultConf ::
  -- | The Size of the Population, must be even
  Int ->
  -- | The number of generations to be run, must be 1 or higher
  Int ->
  GeneticConfiguration
mkDefaultConf pops its = def {populationSize = pops, iterations = its}

--------------------------------------------------------------------------------
----                        Tournament Configuration                        ----
--------------------------------------------------------------------------------

-- Holds all attributes for the tournament selection process
data TournamentConfiguration = TConf
  { -- | how many participants will be in one round of the tournament. Population should be significantly larger than tournament size!
    tSize :: Int,
    -- | how many rounds will one participant do
    tRounds :: Int
  }
  deriving (Eq, Show, Read, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[CamelToSnake]] TournamentConfiguration

instance Default TournamentConfiguration where
  -- TODO: Sensible defaults
  def = TConf 10 5

instance Materializeable TournamentConfiguration where
  data Unmaterialized TournamentConfiguration = UmTournamentConfiguration
    { umTSize :: Maybe Int,
      umTRounds :: Maybe Int
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized TournamentConfiguration)

  conjure = UmTournamentConfiguration Nothing Nothing

  override x Nothing = x
  override TConf {..} (Just UmTournamentConfiguration {..}) =
    TConf
      { tSize = fromMaybe tSize umTSize,
        tRounds = fromMaybe tRounds umTSize
      }

--------------------------------------------------------------------------------
----                          Island Configuration                          ----
--------------------------------------------------------------------------------

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
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[CamelToSnake]] IslandConfiguration

instance Default IslandConfiguration where
  -- TODO: Sensible defaults
  def = IConf 3 5 5 False

instance Materializeable IslandConfiguration where
  data Unmaterialized IslandConfiguration = UmIslandConfiguration
    { umIslands :: Maybe Int,
      umMigrationInterval :: Maybe Int,
      umMigrationSize :: Maybe Int,
      umRingwiseMigration :: Maybe Bool
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized IslandConfiguration)

  conjure = UmIslandConfiguration Nothing Nothing Nothing Nothing

  override x Nothing = x
  override IConf {..} (Just UmIslandConfiguration {..}) =
    IConf
      { islands = fromMaybe islands umIslands,
        migrationInterval = fromMaybe migrationInterval umMigrationInterval,
        migrationSize = fromMaybe migrationSize umMigrationSize,
        ringwiseMigration = fromMaybe ringwiseMigration umRingwiseMigration
      }
