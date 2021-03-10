{-# LANGUAGE RecordWildCards #-}
module Synth.Types where

import GHC
import Constraint
import Outputable
import qualified Outputable as O

-- Properties are strings, for now. We could parse them into LHsExpr GhcPs later.
type RProp = String

-- The context is really just a big let binding which we could parse into a
-- LHsBinds GhcPs
type RContext = [String]

type RType = String
type RExpr = String

type RFix = (LHsDecl GhcPs, LHsDecl GhcPs)

-- A collection of things needed for a problem
data RProblem = RProb { r_props :: [RProp]
                      , r_ctxt :: RContext
                      , r_target :: String
                      , r_ty :: RType
                      , r_prog :: RExpr }

-- ExprFitCands are used by the plugin to check whether an expression could fit
-- a given hole. Since they are not supported within the HoleFit framework, we
-- do it manually in the plugin.
data ExprFitCand = EFC { efc_cand :: LHsExpr GhcTc
                       , efc_wc :: Cts
                       , efc_ids :: [Id]
                       , efc_ty :: Maybe Type }

instance Outputable ExprFitCand where
    ppr EFC{..} = text "EFC {" O.<> ppr efc_cand O.<> text "}"