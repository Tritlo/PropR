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

-- | The materializeable class defines the data we know
-- how to materialize, i.e. bring from undefined data.
-- TODO: We could definitely derive this
class Materializeable a where
  data Unmaterialized a :: *
  materialize :: Maybe (Unmaterialized a) -> a
  conjure :: Unmaterialized a

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

  materialize Nothing = def
  materialize (Just UmConf {..}) =
    Conf
      { compileConfig = materialize umCompileConfig,
        repairConfig = materialize umRepairConfig,
        outputConfig = materialize umOutputConfig,
        logConfig = materialize umLogConfig,
        randomSeed = umRandomSeed,
        searchAlgorithm = materialize umSearchAlgorithm
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

  materialize Nothing = def
  materialize (Just UmGeneticConfiguration {..}) =
    GConf
      { mutationRate = vod mutationRate umMutationRate,
        crossoverRate = vod crossoverRate umCrossoverRate,
        iterations = vod iterations umIterations,
        populationSize = vod populationSize umPopulationSize,
        timeoutInMinutes = vod timeoutInMinutes umTimeoutInMinutes,
        stopOnResults = vod stopOnResults umStopOnResults,
        -- Note: we don't want to conjure a tournament or island config
        -- if there is none present
        tournamentConfiguration = case umTournamentConfiguration of
          Just umt -> Just $ materialize $ Just umt
          _ -> Nothing,
        islandConfiguration = case umIslandConfiguration of
          Just umi -> Just $ materialize $ Just umi
          _ -> Nothing,
        dropRate = vod dropRate umDropRate,
        tryMinimizeFixes = vod tryMinimizeFixes umTryMinimizeFixes,
        replaceWinners = vod replaceWinners umReplaceWinners,
        useParallelMap = vod useParallelMap umUseParallelMap
      }
    where
      vod :: (GeneticConfiguration -> a) -> Maybe a -> a
      vod _ (Just v) = v
      vod g _ = g def
      vod' g (Just v) = Just v
      vod' g _ = g def

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

  materialize Nothing = def
  materialize (Just UmPseudoGeneticConfiguration {..}) =
    PseudoGenConf
      { genIndividuals = vod genIndividuals umGenIndividuals,
        genRounds = vod genRounds umGenRounds,
        genPar = vod genPar umGenPar
      }
    where
      vod :: (PseudoGenConf -> a) -> Maybe a -> a
      vod _ (Just v) = v
      vod g _ = g def

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

  materialize Nothing = def
  materialize (Just UmTournamentConfiguration {..}) =
    TConf (vod size umTSize) (vod rounds umTRounds)
    where
      vod _ (Just v) = v
      vod g _ = g def

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

  materialize Nothing = def
  materialize (Just UmIslandConfiguration {..}) =
    IConf
      { islands = vod islands umIslands,
        migrationInterval = vod migrationInterval umMigrationInterval,
        migrationSize = vod migrationSize umMigrationSize,
        ringwiseMigration = vod ringwiseMigration umRingwiseMigration
      }
    where
      vod _ (Just v) = v
      vod g _ = g def

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

  materialize Nothing = def
  materialize (Just (UmGenetic umc)) =
    Genetic $ materialize $ Just umc
  materialize (Just (UmPseudoGenetic umpgc)) =
    PseudoGenetic $ materialize $ Just umpgc

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

  materialize Nothing = def
  materialize (Just UmOutConf {..}) =
    OutputConf
      { locale = case umLocale of
          Just lcl -> Just lcl
          _ -> locale def,
        directory = fromMaybe (directory def) umDirectory,
        overwrite = fromMaybe (overwrite def) umOverwrite
      }

-- | Parses the given configuration or reads it from file (if it's a file).
-- Retursn a default configuration if none is given.
getConfiguration :: Maybe String -> IO Configuration
getConfiguration Nothing = do
  let conf = materialize (Just conjure)
  seed <- randomIO
  let conf' = conf {randomSeed = Just seed}
  addCliArguments conf'
getConfiguration (Just fp) = do
  fileExists <- doesFileExist fp
  res <-
    if fileExists
      then eitherDecodeFileStrict' fp
      else return $ eitherDecodeStrict' (BS.pack fp)
  case res of
    Left err -> error err
    Right um_conf -> addCliArguments $ materialize um_conf

-- TODO: Implement using opt-parse
addCliArguments :: Configuration -> IO Configuration
addCliArguments conf@Conf {..} = do
  loc <- elem "--log-loc" <$> getArgs
  no_loc <- elem "--no-log-loc" <$> getArgs

  time <- elem "--log-timestamp" <$> getArgs
  no_time <- elem "--no-log-timestamp" <$> getArgs

  args <- Map.fromList . map (second (drop 1) . break (== '=')) <$> getArgs
  let mb_lvl = read <$> (args Map.!? "--log-level")
      mb_file = args Map.!? "--log-file"
      mb_seed = args Map.!? "--seed"

  return $
    conf
      { logConfig =
          logConfig
            { logLoc =
                if loc || no_loc
                  then loc
                  else (logLoc logConfig),
              logLevel = case mb_lvl of
                Just lvl -> lvl
                _ -> (logLevel logConfig),
              logFile = case mb_file of
                Just fp -> Just fp
                _ -> (logFile logConfig),
              logTimestamp =
                if time || no_time
                  then time
                  else (logTimestamp logConfig)
            },
        randomSeed = case mb_seed of
          Just s -> read <$> (Just s)
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

  materialize Nothing = def
  materialize (Just UmCompConf {..}) =
    CompConf
      { importStmts = vod importStmts umImportStmts,
        packages = vod packages umPackages,
        hole_lvl = vod hole_lvl umHoleLvl
      }
    where
      vod _ (Just v) = v
      vod g _ = g def

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

  materialize Nothing = def
  materialize (Just UmRepConf {..}) =
    RepConf
      (fromMaybe (repParChecks def) umParChecks)
      (fromMaybe (repUseInterpreted def) umUseInterpreted)

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

  materialize Nothing = def
  materialize (Just UmLogConf {..}) =
    LogConf
      (fromMaybe (logLoc def) umLogLoc)
      (fromMaybe (logLevel def) umLogLevel)
      (fromMaybe (logTimestamp def) umLogTimestamp)
      umLogFile
