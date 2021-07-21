{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Endemic.Search.PseudoGenetic.Configuration where

import Data.Default
import Data.Maybe
import Deriving.Aeson
import Endemic.Configuration.Materializeable
import GHC.Generics

-- | GenConf represents a set of configurations for the Pseudo Genetic Experiment
data PseudoGenConf = PseudoGenConf
  { -- | The number of individuals in a generation
    genIndividuals :: Int,
    -- | The number of generations processed
    genRounds :: Int,
    -- | Whether or not to use parallelisation in genetic search parts
    genPar :: Bool
  }
  deriving (Show, Eq, Generic, Read)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[CamelToSnake]] PseudoGenConf

instance Default PseudoGenConf where
  def =
    PseudoGenConf
      { genIndividuals = 4,
        genRounds = 5,
        genPar = True
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