{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Endemic.Configuration.Types where

import Data.Aeson
import Data.Default
import Data.Maybe (fromMaybe)
import Data.Time.Format
import Data.Time.LocalTime
import Deriving.Aeson
import Endemic.Search.Genetic.Configuration
import Endemic.Search.PseudoGenetic.Configuration
import Endemic.Types

-- | Logging configuration
data LogConfig = LogConf
  { logLoc :: Bool,
    logLevel :: LogLevel,
    logTimestamp :: Bool,
    logFile :: Maybe String
  }
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake], RejectUnknownFields] LogConfig

-- We need these as well

deriving instance Generic TimeZone

deriving instance ToJSON TimeZone

deriving instance FromJSON TimeZone

deriving instance Generic TimeLocale

deriving instance ToJSON TimeLocale

deriving instance FromJSON TimeLocale

-- | The materializeable class defines the data we know how to materialize, i.e.
-- bring from undefined data, and override
-- TODO: We could definitely derive this
class Default a => Materializeable a where
  data Unmaterialized a :: *
  materialize :: Maybe (Unmaterialized a) -> a
  conjure :: Unmaterialized a
  override :: a -> Maybe (Unmaterialized a) -> a
  materialize = override def

mbOverride :: Maybe a -> Maybe a -> Maybe a
mbOverride _ (Just x) = Just x
mbOverride p _ = p

overrideIfPresent :: Materializeable a => Maybe a -> Maybe (Unmaterialized a) -> Maybe a
overrideIfPresent (Just x) o = Just $ override x o
overrideIfPresent _ x@(Just _) = Just $ materialize x
overrideIfPresent _ _ = Nothing

instance Materializeable a => Default (Unmaterialized a) where
  def = conjure

-- | A configuration contains all the settings
data Configuration = Conf
  { compileConfig :: CompileConfig,
    repairConfig :: RepairConfig,
    outputConfig :: OutputConfig,
    searchAlgorithm :: SearchAlgorithm,
    logConfig :: LogConfig,
    randomSeed :: Maybe Int
  }
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake], RejectUnknownFields] Configuration

instance Default Configuration where
  def =
    Conf
      { compileConfig = def,
        repairConfig = def,
        outputConfig = def,
        searchAlgorithm = def,
        logConfig = def,
        randomSeed = Nothing
      }

-- An unmaterialied config, with possible gaps in it. This is then
-- "materialized" by filling in the gaps
instance Materializeable Configuration where
  data Unmaterialized Configuration = UmConf
    { -- | Configuration for the compilation of programs
      umCompileConfig :: Maybe (Unmaterialized CompileConfig),
      -- | Configuration for the repair of programs
      umRepairConfig :: Maybe (Unmaterialized RepairConfig),
      umOutputConfig :: Maybe (Unmaterialized OutputConfig),
      umLogConfig :: Maybe (Unmaterialized LogConfig),
      -- | Configuration for the genetic repair algorithm.
      --  Left if we're to use the PseudoGenConf, Right otherwise.
      -- Defaults to a Genetic Configuration.
      umSearchAlgorithm :: Maybe (Unmaterialized SearchAlgorithm),
      umRandomSeed :: Maybe Int
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON
            '[ OmitNothingFields,
               RejectUnknownFields,
               FieldLabelModifier '[StripPrefix "um", CamelToSnake]
             ]
            (Unmaterialized Configuration)

  conjure = UmConf n n n n n n
    where
      n = Nothing

  override conf Nothing = conf
  override Conf {..} (Just UmConf {..}) =
    Conf
      { compileConfig = override compileConfig umCompileConfig,
        repairConfig = override repairConfig umRepairConfig,
        outputConfig = override outputConfig umOutputConfig,
        logConfig = override logConfig umLogConfig,
        searchAlgorithm = override searchAlgorithm umSearchAlgorithm,
        randomSeed = mbOverride randomSeed umRandomSeed
      }

-- | Holds the primary switch wether we want to use Genetic Search or BFS,
-- and the assoicated configuration
data SearchAlgorithm
  = Genetic GeneticConfiguration
  | PseudoGenetic PseudoGenConf
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake], RejectUnknownFields] SearchAlgorithm

-- We need all these as well
deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake], OmitNothingFields, RejectUnknownFields] PseudoGenConf instance (FromJSON PseudoGenConf)

deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake], OmitNothingFields, RejectUnknownFields] PseudoGenConf instance (ToJSON PseudoGenConf)

deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake], OmitNothingFields, RejectUnknownFields] GeneticConfiguration instance (FromJSON GeneticConfiguration)

deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake], OmitNothingFields, RejectUnknownFields] GeneticConfiguration instance (ToJSON GeneticConfiguration)

deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake], OmitNothingFields, RejectUnknownFields] IslandConfiguration instance (FromJSON IslandConfiguration)

deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake], OmitNothingFields, RejectUnknownFields] IslandConfiguration instance (ToJSON IslandConfiguration)

deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake], OmitNothingFields, RejectUnknownFields] TournamentConfiguration instance (FromJSON TournamentConfiguration)

deriving via CustomJSON '[FieldLabelModifier '[CamelToSnake], OmitNothingFields, RejectUnknownFields] TournamentConfiguration instance (ToJSON TournamentConfiguration)

instance Default SearchAlgorithm where
  def = Genetic def

-- | All parameters that are passed to the genetic configuration.
-- All Elements are Maybes, if a Nothing is found we pick the defaults.
instance Materializeable GeneticConfiguration where
  data Unmaterialized GeneticConfiguration = UmGeneticConfiguration
    { umMutationRate :: Maybe Double,
      umCrossoverRate :: Maybe Double,
      umIterations :: Maybe Int,
      umPopulationSize :: Maybe Int,
      umTimeoutInMinutes :: Maybe Double,
      umStopOnResults :: Maybe Bool,
      umTournamentConfiguration :: Maybe (Unmaterialized TournamentConfiguration),
      umIslandConfiguration :: Maybe (Unmaterialized IslandConfiguration),
      umDropRate :: Maybe Double,
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

  conjure = UmGeneticConfiguration n n n n n n n n n n n n
    where
      n = Nothing

  override conf Nothing = conf
  override GConf {..} (Just UmGeneticConfiguration {..}) =
    GConf
      { mutationRate = fromMaybe mutationRate umMutationRate,
        crossoverRate = fromMaybe crossoverRate umCrossoverRate,
        iterations = fromMaybe iterations umIterations,
        populationSize = fromMaybe populationSize umPopulationSize,
        timeoutInMinutes = fromMaybe timeoutInMinutes umTimeoutInMinutes,
        stopOnResults = fromMaybe stopOnResults umStopOnResults,
        dropRate = fromMaybe dropRate umDropRate,
        tryMinimizeFixes = fromMaybe tryMinimizeFixes umTryMinimizeFixes,
        replaceWinners = fromMaybe replaceWinners umReplaceWinners,
        useParallelMap = fromMaybe useParallelMap umUseParallelMap,
        tournamentConfiguration = overrideIfPresent tournamentConfiguration umTournamentConfiguration,
        islandConfiguration = overrideIfPresent islandConfiguration umIslandConfiguration
      }

instance Materializeable PseudoGenConf where
  data Unmaterialized PseudoGenConf = UmPseudoGeneticConfiguration
    { umGenIndividuals :: Maybe Int,
      umGenRounds :: Maybe Int,
      umGenPar :: Maybe Bool
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized PseudoGenConf)

  conjure = UmPseudoGeneticConfiguration Nothing Nothing Nothing

  override x Nothing = x
  override PseudoGenConf {..} (Just UmPseudoGeneticConfiguration {..}) =
    PseudoGenConf
      { genIndividuals = fromMaybe genIndividuals umGenIndividuals,
        genRounds = fromMaybe genRounds umGenRounds,
        genPar = fromMaybe genPar umGenPar
      }

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

instance Materializeable SearchAlgorithm where
  data Unmaterialized SearchAlgorithm
    = UmGenetic (Unmaterialized GeneticConfiguration)
    | UmPseudoGenetic (Unmaterialized PseudoGenConf)
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON
            '[ OmitNothingFields,
               RejectUnknownFields,
               ConstructorTagModifier '[StripPrefix "Um"],
               FieldLabelModifier '[StripPrefix "um", CamelToSnake]
             ]
            (Unmaterialized SearchAlgorithm)

  conjure = UmGenetic conjure

  override c Nothing = c
  override (Genetic _) (Just (UmPseudoGenetic psc)) =
    PseudoGenetic $ materialize $ Just psc
  override (PseudoGenetic _) (Just (UmGenetic gc)) =
    Genetic $ materialize $ Just gc
  override (Genetic conf) (Just (UmGenetic gc)) =
    Genetic $ override conf (Just gc)
  override (PseudoGenetic conf) (Just (UmPseudoGenetic pgc)) =
    PseudoGenetic $ override conf (Just pgc)

-- | Configuration for the output
data OutputConfig = OutputConf
  { -- | The users locale, for complete reproduction
    locale :: Maybe TimeLocale,
    -- | Which directory to write the patches to.
    directory :: FilePath,
    -- | Whether to overwrite previous patches
    overwrite :: Bool,
    -- | Whether to save the patches
    savePatches :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake], RejectUnknownFields] OutputConfig

instance Default OutputConfig where
  def =
    OutputConf
      { locale = Nothing,
        directory = "./output-patches-",
        overwrite = False,
        savePatches = True
      }

instance Materializeable OutputConfig where
  data Unmaterialized OutputConfig = UmOutConf
    { umLocale :: Maybe TimeLocale,
      umDirectory :: Maybe FilePath,
      umOverwrite :: Maybe Bool,
      umSavePatches :: Maybe Bool
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized OutputConfig)

  conjure = UmOutConf Nothing Nothing Nothing Nothing

  override c Nothing = c
  override OutputConf {..} (Just UmOutConf {..}) =
    OutputConf
      { locale = mbOverride locale umLocale,
        directory = fromMaybe directory umDirectory,
        overwrite = fromMaybe overwrite umOverwrite,
        savePatches = fromMaybe savePatches umSavePatches
      }

instance Materializeable CompileConfig where
  data Unmaterialized CompileConfig = UmCompConf
    { umImportStmts :: Maybe [String],
      umPackages :: Maybe [String],
      umHoleLvl :: Maybe Int,
      umQcSeed  :: Maybe Integer
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized CompileConfig)

  conjure = UmCompConf Nothing Nothing Nothing Nothing

  override c Nothing = c
  override CompConf {..} (Just UmCompConf {..}) =
    CompConf
      { importStmts = fromMaybe importStmts umImportStmts,
        packages = fromMaybe packages umPackages,
        hole_lvl = fromMaybe hole_lvl umHoleLvl,
        qcSeed = mbOverride qcSeed umQcSeed
      }

-- | Configuration for the compilation itself
data CompileConfig = CompConf
  { -- | a list of imports required/wanted for the compilation
    importStmts :: [String],
    -- | a list of packages used for the compilation
    packages :: [String],
    -- | the "depth" of the holes, see general notes on this
    hole_lvl :: Int,
    -- | The seed to use for quickcheck
    qcSeed :: Maybe Integer
  }
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake], RejectUnknownFields] CompileConfig

instance Default CompileConfig where
  def =
    CompConf
      { hole_lvl = 0,
        packages = ["base", "process", "check-helpers", "QuickCheck"],
        importStmts = [ "import Prelude" ],
        qcSeed = Nothing
      }

-- | Configuration for the checking of repairs
data RepairConfig = RepConf
  { -- | Whether or not to use Parallelisation
    repParChecks :: Bool,
    -- | Whether or not to use compiled sources (?)
    repUseInterpreted :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "rep", CamelToSnake], RejectUnknownFields] RepairConfig

instance Default RepairConfig where
  def =
    RepConf
      { repParChecks = True,
        repUseInterpreted = True
      }

instance Materializeable RepairConfig where
  data Unmaterialized RepairConfig = UmRepConf
    { umParChecks :: Maybe Bool,
      umUseInterpreted :: Maybe Bool
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized RepairConfig)

  conjure = UmRepConf Nothing Nothing

  override c Nothing = c
  override RepConf {..} (Just UmRepConf {..}) =
    RepConf
      { repParChecks = fromMaybe repParChecks umParChecks,
        repUseInterpreted = fromMaybe repUseInterpreted umUseInterpreted
      }

instance Default LogConfig where
  def = LogConf {logLoc = False, logLevel = WARN, logFile = Nothing, logTimestamp = True}

instance Materializeable LogConfig where
  data Unmaterialized LogConfig = UmLogConf
    { umLogLoc :: Maybe Bool,
      umLogLevel :: Maybe LogLevel,
      umLogTimestamp :: Maybe Bool,
      umLogFile :: Maybe String
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized LogConfig)

  conjure = UmLogConf Nothing Nothing Nothing Nothing

  override c Nothing = c
  override LogConf {..} (Just UmLogConf {..}) =
    LogConf
      { logLoc = fromMaybe logLoc umLogLoc,
        logLevel = fromMaybe logLevel umLogLevel,
        logTimestamp = fromMaybe logTimestamp umLogTimestamp,
        logFile = mbOverride logFile umLogFile
      }

-- | The Problem Description is generated at runtime, descriping a particular
-- program to fix.
data ProblemDescription = ProbDesc
  { progProblem :: EProblem,
    exprFitCands :: [ExprFitCand],
    compConf :: CompileConfig,
    repConf :: RepairConfig
  }
