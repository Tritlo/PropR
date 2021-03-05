module Synth.Types where

import GHC

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