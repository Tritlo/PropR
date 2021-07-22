{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Endemic.Search.Random.Configuration where

import Data.Default
import Data.Maybe (fromMaybe)
import Deriving.Aeson
import Endemic.Configuration.Materializeable
import GHC.Generics

--------------------------------------------------------------------------------
----                      Configuration                                   ------
--------------------------------------------------------------------------------

data RandomConf = RandConf
  { -- | Random budget in seconds
    randSearchBudget :: Int,
    randMaxFixSize :: Int,
    randIgnoreFailing :: Bool,
    randStopOnResults :: Bool
  }
  deriving (Show, Eq, Generic, Read)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[CamelToSnake]] RandomConf

instance Default RandomConf where
  def =
    RandConf
      { randSearchBudget = 5 * 60,
        randMaxFixSize = 10,
        randIgnoreFailing = False,
        randStopOnResults = False
      }

instance Materializeable RandomConf where
  data Unmaterialized RandomConf = UmRandomRepairConfiguration
    { umRandSearchBudget :: Maybe Int,
      umRandMaxFixSize :: Maybe Int,
      umRandIgnoreFailing :: Maybe Bool,
      umRandStopOnResults :: Maybe Bool
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized RandomConf)

  conjure = UmRandomRepairConfiguration Nothing Nothing Nothing Nothing

  override x Nothing = x
  override RandConf {..} (Just UmRandomRepairConfiguration {..}) =
    RandConf
      { randSearchBudget = fromMaybe randSearchBudget umRandSearchBudget,
        randMaxFixSize = fromMaybe randMaxFixSize umRandMaxFixSize,
        randIgnoreFailing = fromMaybe randIgnoreFailing umRandIgnoreFailing,
        randStopOnResults = fromMaybe randStopOnResults umRandStopOnResults
      }
