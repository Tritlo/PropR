{-# LANGUAGE TypeFamilies #-}

module PropR.Configuration.Materializeable where

import Data.Default
import Data.Kind (Type)

-- | The materializeable class defines the data we know how to materialize, i.e.
-- bring from undefined data, and override
-- TODO: We could definitely derive this
class Default a => Materializeable a where
  data Unmaterialized a :: Type
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
