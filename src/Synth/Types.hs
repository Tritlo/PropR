module Synth.Types where


-- Properties are strings, for now. We could parse them into LHsExpr GhcPs later.
type RProp = String

-- The context is really just a big let binding which we could parse into a
-- LHsBinds GhcPs
type RContext = [String]

type RType = String
type RExpr = String
