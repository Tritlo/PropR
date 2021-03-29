{-# LANGUAGE RecordWildCards #-}

module Synth.Types where

import Constraint
import GHC
import Outputable
import qualified Outputable as O

-- Properties are strings, for now. We could parse them into LHsExpr GhcPs later.
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

data EProblem = EProb
  { e_props :: [EProp],
    e_ctxt :: EContext,
    e_target :: String,
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