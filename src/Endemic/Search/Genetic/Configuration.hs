{-# LANGUAGE RecordWildCards #-}
module Endemic.Search.Genetic.Configuration where

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


-- ===========                 ==============
-- ===      Genetic Configurations        ===
-- ===========                 ==============

-- | The GeneticConfiguration holds all elements and Flags for the genetic search,
-- Used to slim down Signatures and provide a central lookup-point.
data GeneticConfiguration = GConf
  { mutationRate :: Double         -- ^ The chance that any one element is mutated
  , crossoverRate :: Double        -- ^ the chance that a crossover is performed per parent pair
  , iterations :: Int              -- ^ How many iterations to do max (or exit earlier, depending on "stopOnResults")
  , populationSize :: Int          -- ^ How many Chromosomes are in one Population. In case of Island Evolution, each Island will have this population.
  , timeoutInMinutes :: Double     -- ^ How long the process should run (in Minutes)
  , stopOnResults :: Bool          -- ^ Whether or not to stop at the generation that first produces positive results
  , tournamentConfiguration :: Maybe TournamentConfiguration -- ^ Nothing to not do Tournament Selection, existing Conf will use Tournament instead (See below for more info)
  , islandConfiguration :: Maybe IslandConfiguration -- ^ Nothing to disable IslandEvolution, existing Conf will run Island Evolution (See below for more Info)
  -- Pick better names for these? :
  , dropRate :: Double              -- ^ The probability of how often we drop during mutation
  , progProblem :: EProblem         -- ^ The problem we're trying to solve
  , compConf :: CompileConfig       -- ^ The compiler configuration, required to retrieve mutated EFixes
  , exprFitCands :: [ExprFitCand]   -- ^ The sum of all potentially replaced elements, required to retrieve mutated EFixes

  , tryMinimizeFixes :: Bool        -- ^ Whether or not to try to minimize the successfull fixes. This step is performed after search as postprocessing and does not affect initial search runtime.
  , replaceWinners :: Bool          -- ^ Whether successfull candidates will be removed from the populations, replaced with a new-full-random element.
  , useParallelMap :: Bool          -- ^ Whether to use as much parallelism as possible
  }

mkDefaultConf ::
    Int -- ^ The Size of the Population, must be even
    -> Int -- ^ The number of generations to be run, must be 1 or higher
    -> EProblem -> CompileConfig -> [ExprFitCand] -> GeneticConfiguration
mkDefaultConf pops its prob cc ecands = GConf {..}
    where mutationRate = 0.2
          crossoverRate = 0.05
          iterations = its
          populationSize = pops
          timeoutInMinutes = 5
          stopOnResults = True
          tournamentConfiguration = Nothing
          islandConfiguration = Nothing
          -- T
          dropRate = 0.2
          progProblem = prob
          compConf = cc
          exprFitCands = ecands
          tryMinimizeFixes = False  -- Not implemented
          replaceWinners = True
          useParallelMap = True



-- Holds all attributes for the tournament selection process
data TournamentConfiguration = TConf {
    size :: Int,        -- ^ how many participants will be in one round of the tournament. Population should be significantly larger than tournament size!
    rounds :: Int       -- ^ how many rounds will one participant do
}

-- | Holds all attributes required to perform an Island Evolution.
-- For more Information on Island Evolution see https://neo.lcc.uma.es/Articles/WRH98.pdf
data IslandConfiguration = IConf {
    islands :: Int,                 -- ^ How many Islands are in place, each will have a population according to
    migrationInterval :: Int ,      -- ^ After how many generations will there be an Island Migration
    -- This is often done until a full circle is complete, that means that you did islands * migrationInterval generations.
    -- We keep it in mind but do not enforce it.
    -- TODO: Make a note about this in Configuration Setup
    -- TODO: Make a note on using this only for big programs / search spaces, otherwise the resources are maybe not worth it.
    migrationSize :: Int ,           -- ^ How many Chromosomes will make it from one Island to another
    ringwiseMigration :: Bool        -- ^ Whether the migration is done clockwise, True for ringwise, False for random pairs of migration
}
