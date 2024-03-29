-- |
-- Module      : PropR.configuration
-- Description : This module reads in configurations necessary for the project
-- License     : MIT
-- Stability   : experimental
--
-- This module uses the materializable class to read JSON configuration.
-- Most of the Code follows Schema F: We implement the class for all configuration(pieces), only "getConfiguration" and "addCLIArguments" are not in that Schema.
--
-- Notes:
-- Conjure is a wild name, but it just means that we need a default configuration. For the default file-read configuration, this means an empty object.
-- Materialize means to read it in, however our reading in is highly automized using Aeson.
module PropR.Configuration
  ( -- Datatypes
    Configuration (..),
    OutputConfig (..),
    CompileConfig (..),
    LogConfig (..),
    SearchAlgorithm (..),
    -- Global flags
    setGlobalFlags,
    lOGCONFIG,
    -- Generating good random seeds for quickcheck
    newSeed,
    setSeedGenSeed,
    withFrozenSeedGen,
    -- Configuration
    CLIOptions (..),
    getConfiguration,
    getConfiguration',
    ProblemDescription (..),
    module PropR.Configuration.Types,
  )
where

import PropR.Configuration.Configure
import PropR.Configuration.Types
