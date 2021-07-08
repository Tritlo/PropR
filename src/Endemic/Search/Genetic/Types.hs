{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Endemic.Search.Genetic.Types where

import Control.Monad(when, replicateM)
import System.Random
import Data.Maybe
import Data.List(sortBy,delete, groupBy, sortOn, partition)
import Data.Time.Clock

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Endemic.Types (EFix, EProblem (EProb, e_prog, e_ty), CompileConfig, ExprFitCand)
import GHC (SrcSpan, HsExpr, GhcPs, isSubspanOf)
import qualified Data.Map as Map
import Data.Function (on)
import GhcPlugins (HasCallStack,ppr, showSDocUnsafe, liftIO, getOrigNameCache, CompilerInfo (UnknownCC), Outputable(..))

import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as ST
import Control.Monad.Trans.Class (lift)
import Endemic.Repair (repairAttempt)
import Endemic.Util
import Endemic.Traversals (replaceExpr)
import Endemic.Eval (checkFixes)
import qualified Data.Set as Set

import Control.Monad.Par.IO (IVar, ParIO, runParIO)
import Control.Monad.Par.Combinator (parMapM)
import Control.DeepSeq (NFData(..), rwhnf)
import Control.Concurrent.Async

import Endemic.Search.Genetic.Configuration

-- ===========                                    ==============
-- ===                  Genetic Requirements                 ===
-- === All Instances and Setups required for genetic search  ===
-- ===========                                    ==============


-- Eq is required to remove elements from lists while partitioning.
class (Eq g, Outputable g, NFData g) => Chromosome g where
    -- | TODO: We could also move the crossover to the configuration
    crossover :: (g,g) -> GenMonad (g,g) -- ^ The Crossover Function to produce a new Chromosome from two Genes. This Crossover must "always hit", taking care of whether to do crossover or not is done in genetic search.
    mutate :: g -> GenMonad g            -- ^ The Mutation Function, in a seeded Fashion. This is a mutation that always "hits", taking care of not mutating every generation is done in genetic search.

    -- | TODO: Do we want to move Fitness out,
    -- and just require a Function (Fitness :: Chromosome g, Ord a => g -> a) in the Method Signatures?
    -- Or do we "bloat" our signatures heavily if we always carry it around?
    -- Optionally, we can put it into the Configuration
    fitness :: g -> GenMonad Double                            -- ^ A fitness function, applicable to the Chromosome.


    -- | Returns an Initial Population of Size p
    initialPopulation ::
           Int                                     -- ^ The size of the population
        -> GenMonad [g]                            -- ^ The first population, represented as a list of genes.

    -- To improve performance, we allow users to overwrite these in case
    -- they can be implemented more efficiently
    fitnessMany :: [g] -> GenMonad [Double]
    fitnessMany = mapM fitness

    mutateMany :: [g] -> GenMonad [g]
    mutateMany = mapM mutate

    crossoverMany :: [(g,g)] -> GenMonad [(g,g)]
    crossoverMany = mapM crossover

-- | This FitnessCache is created to hold the known fitness values of Efixes.
-- When the fitness function is called, it performs a lookup here.
-- In it's current implementation, the Cache is updated at the mutate step, when a new EFix is created.
-- In it's current implementation, the Cache is never cleared.
type FitnessCache = Map.Map EFix Double

-- | The GenMonad resembles the environment in which we run our Genetic Search and it's parts.
-- It was introduced to reduce the load on various signatures and provide caching easier.
-- It consists (in this order) of
-- - A Read only GeneticConfiguration
-- - A Read-Write random number provider
-- - A Read-Write cache for Fitness values
-- - IO, to perform logging and Time-Tasks
-- The order of these is not particularly important, but we moved them in order of their occurrence (that is,
-- configuration is used the most, while IO and caching are used the least)
type GenMonad = R.ReaderT GeneticConfiguration (ST.StateT StdGen (ST.StateT FitnessCache IO))
