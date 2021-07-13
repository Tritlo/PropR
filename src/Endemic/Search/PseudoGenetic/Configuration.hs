{-# LANGUAGE DeriveGeneric #-}

module Endemic.Search.PseudoGenetic.Configuration where

import Data.Default
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

instance Default PseudoGenConf where
  def =
    PseudoGenConf
      { genIndividuals = 4,
        genRounds = 5,
        genPar = True
      }
