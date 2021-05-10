{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Synth.Types
Description : Holds the types used throughout the HenProg Library
License     : MIT
Stability   : experimental

This module holds common / shared types throughout the HenProg library. 
Prefixes / Naming Convention: 
- "R": 
- "E":
- "Ghc": Glasgow Haskell Compiler 
- "LHs": Located Haskell, e.g. Located-Haskell-Binding. Located does not necessarily mean "local", 
        it refers to a piece of Haskell code at a certain known position in a program, in contrag e.g. to the synthesized fixed which are un-located at first. 
- "Ps": Parsed, as a pass in GHC before compilation
- "Ty": Type
- "Wc": WildCard, for our purposes the "holes"

TODO: Difference between R & E ? 
R for Readerbased == Strings and E for Expression-Based == Compiled & Resolved? 
-}
module Synth.Types where

import Constraint
import Data.Map (Map)
import GHC
import Outputable
import qualified Outputable as O

{-|
-- Properties as in QuickCheck Properties.
-- Properties are strings, for now. We could parse them into LHsExpr GhcPs later.
-}
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

-- A fix is a list of replacements and their locations
type EFix = Map SrcSpan (HsExpr GhcPs)

data EProblem = EProb
  { e_props :: [EProp],
    e_ctxt :: EContext,
    e_target :: RdrName,
    e_ty :: EType,
    e_prog :: EExpr
  }

-- ExprFitCands are used by the plugin to check whether an expression could fit
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