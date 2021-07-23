{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Endemic.Search.Exhaustive.Configuration where

import Data.Default
import Data.Maybe (fromMaybe)
import Deriving.Aeson
import Endemic.Configuration.Materializeable
import GHC.Generics

--------------------------------------------------------------------------------
----                      Configuration                                   ------
--------------------------------------------------------------------------------

data ExhaustiveConf = ExhaustiveConf
  { -- | Exhaustive budget in seconds
    exhSearchBudget :: Int,
    exhIgnoreFailing :: Bool,
    exhStopOnResults :: Bool
  }
  deriving (Show, Eq, Generic, Read)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[CamelToSnake]] ExhaustiveConf

instance Default ExhaustiveConf where
  def =
    ExhaustiveConf
      { exhSearchBudget = 5 * 60,
        exhIgnoreFailing = True,
        exhStopOnResults = False
      }

instance Materializeable ExhaustiveConf where
  data Unmaterialized ExhaustiveConf = UmExhaustiveRepairConfiguration
    { umExhaustiveSearchBudget :: Maybe Int,
      umExhaustiveStopOnResults :: Maybe Bool,
      umExhaustiveIgnoreFailing :: Maybe Bool
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized ExhaustiveConf)

  conjure = UmExhaustiveRepairConfiguration Nothing Nothing Nothing

  override x Nothing = x
  override ExhaustiveConf {..} (Just UmExhaustiveRepairConfiguration {..}) =
    ExhaustiveConf
      { exhSearchBudget = fromMaybe exhSearchBudget umExhaustiveSearchBudget,
        exhStopOnResults = fromMaybe exhStopOnResults umExhaustiveStopOnResults,
        exhIgnoreFailing = fromMaybe exhIgnoreFailing umExhaustiveIgnoreFailing
      }
