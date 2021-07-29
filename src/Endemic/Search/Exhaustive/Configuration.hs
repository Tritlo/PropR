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
    -- | Whether to exit early on the first results, or to use the full search budget.
    exhStopOnResults :: Bool,
    -- | How many fixes to check at once
    exhBatchSize :: Int
  }
  deriving (Show, Eq, Generic, Read)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[CamelToSnake]] ExhaustiveConf

instance Default ExhaustiveConf where
  def =
    ExhaustiveConf
      { exhSearchBudget = 5 * 60,
        exhStopOnResults = False,
        exhBatchSize = 10
      }

instance Materializeable ExhaustiveConf where
  data Unmaterialized ExhaustiveConf = UmExhaustiveRepairConfiguration
    { umExhSearchBudget :: Maybe Int,
      umExhStopOnResults :: Maybe Bool,
      umExhBatchSize :: Maybe Int
    }
    deriving (Show, Eq, Generic)
    deriving
      (FromJSON, ToJSON)
      via CustomJSON '[OmitNothingFields, RejectUnknownFields, FieldLabelModifier '[StripPrefix "um", CamelToSnake]] (Unmaterialized ExhaustiveConf)

  conjure = UmExhaustiveRepairConfiguration Nothing Nothing Nothing

  override x Nothing = x
  override ExhaustiveConf {..} (Just UmExhaustiveRepairConfiguration {..}) =
    ExhaustiveConf
      { exhSearchBudget = fromMaybe exhSearchBudget umExhSearchBudget,
        exhStopOnResults = fromMaybe exhStopOnResults umExhStopOnResults,
        exhBatchSize = fromMaybe exhBatchSize umExhBatchSize
      }
