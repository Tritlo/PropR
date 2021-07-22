{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
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
import Endemic.Configuration.Materializeable
import Endemic.Search.Genetic.Configuration
import Endemic.Search.PseudoGenetic.Configuration
import Endemic.Search.Random.Configuration
import Endemic.Types
import GHC (ParsedModule)

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
  | Random RandomConf
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake], RejectUnknownFields] SearchAlgorithm

instance Default SearchAlgorithm where
  def = Genetic def

instance Materializeable SearchAlgorithm where
  data Unmaterialized SearchAlgorithm
    = UmGenetic (Unmaterialized GeneticConfiguration)
    | UmPseudoGenetic (Unmaterialized PseudoGenConf)
    | UmRandom (Unmaterialized RandomConf)
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
  override (Genetic conf) (Just (UmGenetic gc)) =
    Genetic $ override conf (Just gc)
  override (PseudoGenetic conf) (Just (UmPseudoGenetic pgc)) =
    PseudoGenetic $ override conf (Just pgc)
  override (Random conf) (Just (UmRandom pgc)) =
    Random $ override conf (Just pgc)
  override _ (Just (UmPseudoGenetic psc)) =
    PseudoGenetic $ materialize $ Just psc
  override _ (Just (UmGenetic gc)) =
    Genetic $ materialize $ Just gc
  override _ (Just (UmRandom r)) =
    Random $ materialize $ Just r

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
        packages = ["base"],
        importStmts = ["import Prelude"]
      }

-- | Configuration for the checking of repairs
data RepairConfig = RepConf
  { -- | Whether or not to use Parallelisation
    repParChecks :: Bool,
    -- | Whether or not to use bytecode or to
    -- just interpret the resulting code.
    -- Usuallly safe to set to true, except
    -- when Core-to-Core plugins are involved.
    repUseInterpreted :: Bool,
    -- | Set the timeout in microseconds for each
    -- heck, after which we assume the check is
    -- in an infinte loop.
    repTimeout :: Integer
  }
  deriving (Show, Eq, Generic)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "rep", CamelToSnake], RejectUnknownFields] RepairConfig

instance Default RepairConfig where
  def =
    RepConf
      { repParChecks = True,
        repUseInterpreted = True,
        repTimeout = 1_000_000
      }

instance Materializeable RepairConfig where
  data Unmaterialized RepairConfig = UmRepConf
    { umParChecks :: Maybe Bool,
      umUseInterpreted :: Maybe Bool,
      umTimeout :: Maybe Integer
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized RepairConfig)

  conjure = UmRepConf Nothing Nothing Nothing

  override c Nothing = c
  override RepConf {..} (Just UmRepConf {..}) =
    RepConf
      { repParChecks = fromMaybe repParChecks umParChecks,
        repUseInterpreted = fromMaybe repUseInterpreted umUseInterpreted,
        repTimeout = fromMaybe repTimeout umTimeout
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
    repConf :: RepairConfig,
    -- | The parsed module, if available
    probModule :: Maybe ParsedModule
  }

setProg :: ProblemDescription -> EExpr -> ProblemDescription
setProg desc@ProbDesc {progProblem = pp} prog = desc {progProblem = pp {e_prog = prog}}

-- Inline version of setProg
(~>) :: ProblemDescription -> EExpr -> ProblemDescription
(~>) = setProg