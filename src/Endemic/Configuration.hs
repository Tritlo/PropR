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

-- |
-- Module      : Endemic.configuration
-- Description : This module reads in configurations necessary for the project
-- License     : MIT
-- Stability   : experimental
--
-- This module uses the materializable class to read JSON configuration.
-- Most of the Code follows Schema F: We implement the class for all configuration(pieces), only "getConfiguration" and "addCLIArguments" are not in that Schema.
-- TODO: Is the logging done here? Would we want to have logging CLI only?
--
-- Notes:
-- Conjure is a wild name, but it just means that we need a default configuration. For the default file-read configuration, this means an empty object.
-- Materialize means to read it in, however our reading in is highly automized using Aeson.
module Endemic.Configuration
  ( getConfiguration,
    Configuration (..),
    OutputConfig (..),
    CompileConfig (..),
    LogConfig (..),
    RepairConfig (..),
    SearchAlgorithm (..),
    -- Global flags
    setGlobalFlags,
    lOGCONFIG,
  )
where

import Data.Aeson
import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toUpper)
import Data.Default
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Time.Format
import Data.Time.LocalTime
import Deriving.Aeson
import Deriving.Aeson.Stock
import Endemic.Search.Genetic.Configuration
import Endemic.Search.PseudoGenetic.Configuration
import Endemic.Types
import GHC.Generics
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO.Unsafe
import System.Random (randomIO)

-- | Global variable to configure logging
{-# NOINLINE lOGCONFIG #-}
lOGCONFIG :: IORef LogConfig
lOGCONFIG = unsafePerformIO $ newIORef def

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
    overwrite :: Bool
  }
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake], RejectUnknownFields] OutputConfig

instance Default OutputConfig where
  def =
    OutputConf
      { locale = Nothing,
        directory = "./output",
        overwrite = True
      }

instance Materializeable OutputConfig where
  data Unmaterialized OutputConfig = UmOutConf
    { umLocale :: Maybe TimeLocale,
      umDirectory :: Maybe FilePath,
      umOverwrite :: Maybe Bool
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized OutputConfig)

  conjure = UmOutConf Nothing Nothing Nothing

  override c Nothing = c
  override OutputConf {..} (Just UmOutConf {..}) =
    OutputConf
      { locale = mbOverride locale umLocale,
        directory = fromMaybe directory umDirectory,
        overwrite = fromMaybe overwrite umOverwrite
      }

readConf :: String -> IO (Unmaterialized Configuration)
readConf fp = do
  fileExists <- doesFileExist fp
  res <-
    if fileExists
      then eitherDecodeFileStrict' fp
      else return $ eitherDecodeStrict' (BS.pack fp)
  case res of
    Left err -> error err
    Right um_conf -> return um_conf

-- | Parses the given configuration or reads it from file (if it's a file).
-- Retursn a default configuration if none is given.
getConfiguration :: Maybe String -> IO Configuration
getConfiguration Nothing = do
  let conf = materialize (Just conjure)
  seed <- randomIO
  let conf' = conf {randomSeed = Just seed}
  addCliArguments conf'
getConfiguration (Just fp) = do
  readConf fp >>= addCliArguments . materialize . Just

-- TODO: Implement using opt-parse
addCliArguments :: Configuration -> IO Configuration
addCliArguments conf = do
  loc <- elem "--log-loc" <$> getArgs
  no_loc <- elem "--no-log-loc" <$> getArgs

  time <- elem "--log-timestamp" <$> getArgs
  no_time <- elem "--no-log-timestamp" <$> getArgs

  args <- Map.fromList . map (second (drop 1) . break (== '=')) <$> getArgs
  let mb_lvl = read <$> (args Map.!? "--log-level")
      mb_file = args Map.!? "--log-file"
      mb_seed = args Map.!? "--seed"

  conf'@Conf {..} <-
    case args Map.!? "--override" of
      Nothing -> return conf
      Just c -> do
        nc <- readConf c
        return $ override conf (Just nc)
  return $
    conf'
      { logConfig =
          logConfig
            { logLoc =
                if loc || no_loc
                  then loc
                  else logLoc logConfig,
              logLevel = case mb_lvl of
                Just lvl -> lvl
                _ -> logLevel logConfig,
              logFile = case mb_file of
                Just fp -> Just fp
                _ -> logFile logConfig,
              logTimestamp =
                if time || no_time
                  then time
                  else logTimestamp logConfig
            },
        randomSeed = case mb_seed of
          Just s -> read <$> Just s
          _ -> randomSeed
      }

-- | Set global flags sets the global flags to the values specified in
-- configuration, i.e. the `lOGLOC`, `lOGLEVEL` and `dEBUG`.
setGlobalFlags :: Configuration -> IO ()
setGlobalFlags Conf {logConfig = lc} = writeIORef lOGCONFIG lc

instance Materializeable CompileConfig where
  data Unmaterialized CompileConfig = UmCompConf
    { umImportStmts :: Maybe [String],
      umPackages :: Maybe [String],
      umHoleLvl :: Maybe Int
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized CompileConfig)

  conjure = UmCompConf Nothing Nothing Nothing

  override c Nothing = c
  override CompConf {..} (Just UmCompConf {..}) =
    CompConf
      { importStmts = fromMaybe importStmts umImportStmts,
        packages = fromMaybe packages umPackages,
        hole_lvl = fromMaybe hole_lvl umHoleLvl
      }

-- | Configuration for the compilation itself
data CompileConfig = CompConf
  { -- | a list of imports required/wanted for the compilation
    importStmts :: [String],
    -- | a list of packages used for the compilation
    packages :: [String],
    -- | the "depth" of the holes, see general notes on this
    hole_lvl :: Int
  }
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake], RejectUnknownFields] CompileConfig

instance Default CompileConfig where
  def =
    CompConf
      { hole_lvl = 0,
        packages = ["base", "process", "QuickCheck"],
        importStmts = ["import Prelude"]
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
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "umLog", CamelToSnake]] (Unmaterialized LogConfig)

  conjure = UmLogConf Nothing Nothing Nothing Nothing

  override c Nothing = c
  override LogConf {..} (Just UmLogConf {..}) =
    LogConf
      { logLoc = fromMaybe logLoc umLogLoc,
        logLevel = fromMaybe logLevel umLogLevel,
        logTimestamp = fromMaybe logTimestamp umLogTimestamp,
        logFile = mbOverride logFile umLogFile
      }
