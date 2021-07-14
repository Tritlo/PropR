{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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

import Constraint (Cts)
import Data.Aeson
import Data.Default
import Data.Map (Map)
import GHC
import GHC.Generics
import Outputable (Outputable (ppr), text)
import qualified Outputable as O

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

data LogLevel
  = TRACE
  | DEBUG
  | AUDIT
  | INFO
  | WARN
  | ERROR
  | FATAL
  deriving (Eq, Ord, Read, Show, Generic, FromJSON, ToJSON)

instance Default LogLevel where
  def = WARN
