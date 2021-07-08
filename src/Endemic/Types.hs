{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Endemic.Types
-- Description : Holds the types used throughout the Endemic Library
-- License     : MIT
-- Stability   : experimental
--
-- This module holds common / shared types throughout the Endemic library.
-- Prefixes / Naming Convention:
-- - "R": Unparsed Elements, given by properties e.g. from console input or in
--        the first steps of test-evaluation
-- - "E": Expressions, as provided from GHC.
-- - "Ghc": Glasgow Haskell Compiler
-- - "LHs": Located Haskell, e.g. Located-Haskell-Binding. Located does not
--          necessarily mean "local", it refers to a piece of Haskell code at a
--          certain known position in a program, in contrag e.g. to the
--          synthesized fixed which are un-located at first.
-- - "Ps": Parsed, as a pass in GHC before compilation
-- - "Ty": Type
-- - "Wc": WildCard, for our purposes the "holes"
module Endemic.Types where

import Constraint
import Data.Map (Map)
import Data.Vector (Vector)
import Data.Word (Word32)
import GHC
import Outputable
import qualified Outputable as O
import System.Random.MWC (Seed)

-- |
-- Properties as in QuickCheck Properties.  Properties are strings, for now. We
-- parse them into LHsExpr GhcPs (=Expressions) later.
type RProp = String

type RContext = [String]

type RType = String

type RExpr = String

type RFix = (LHsDecl GhcPs, LHsDecl GhcPs)

-- A collection of things needed for a problem
data RProblem = RProb
  { r_props :: [RProp],
    r_ctxt :: RContext,
    r_target :: String,
    r_ty :: RType,
    r_prog :: RExpr
  }

type EProp = LHsBind GhcPs

type EContext = LHsLocalBinds GhcPs

type EType = LHsSigWcType GhcPs

type EExpr = LHsExpr GhcPs

-- | A fix is a list of replacements and their locations
type EFix = Map SrcSpan (HsExpr GhcPs)

data EProblem = EProb
  { e_props :: [EProp],
    e_ctxt :: EContext,
    e_target :: RdrName,
    e_ty :: EType,
    e_prog :: EExpr
  }

-- | ExprFitCands are used by the plugin to check whether an expression could fit
-- a given hole. Since they are not supported within the HoleFit framework, we
-- do it manually in the plugin.
data ExprFitCand = EFC
  { efc_cand :: LHsExpr GhcTc,
    efc_wc :: Cts,
    efc_ids :: [Id],
    efc_ty :: Maybe Type
  }

instance Outputable ExprFitCand where
  ppr EFC {..} = text "EFC {" O.<> ppr efc_cand O.<> text "}"

data CompileConfig = CompConf
  { -- | a list of imports required/wanted for the compilation
    importStmts :: [String],
    -- | a list of packages used for the compilation
    packages :: [String],
    -- | the "depth" of the wholes, see general notes on this
    hole_lvl :: Int,
    -- | The Configuration for the Genetic Algorithm
    pseudoGenConf :: PseudoGenConf,
    -- | The Configuration for the Repair
    repConf :: RepConf
  }
  deriving (Show, Eq, Read)

data RepConf = RepConf
  { -- | Whether or not to use Parallelisation
    repParChecks :: Bool,
    -- | Whether or not to use compiled sources (?)
    repUseInterpreted :: Bool
  }
  deriving (Show, Eq, Read)

-- | GenConf represents a set of configurations for the Pseudo Genetic Experiment
data PseudoGenConf = PseudoGenConf
  { -- | The number of individuals in a generation
    genIndividuals :: Int,
    -- | The number of generations processed
    genRounds :: Int,
    -- | Whether or not to use parallelisation in genetic search parts
    genPar :: Bool
  }
  deriving (Show, Eq, Read)
