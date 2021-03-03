{-# LANGUAGE RecordWildCards #-}
module Synth.Flatten where

import GHC
import Bag

import Synth.Util

import GhcPlugins (ppr, showSDocUnsafe)

-- A naive implementation of flatten, works for very limited cases so far.
-- We'll need to walk over the entire Haskell AST eventually. We'd like for
-- this to maintain parity with Fill, but this is only used for tests currently.
flattenExpr :: LHsExpr GhcPs -> [LHsExpr GhcPs]
flattenExpr e@(L loc (HsApp x l r)) = e:(flattenExpr l ++ flattenExpr r)
flattenExpr e@(L loc (HsPar x l)) =
    e:(flattenExpr l)
flattenExpr e@(L loc (ExprWithTySig x l t)) =
    e:(flattenExpr l)
flattenExpr e@((L loc (HsLet x b w))) =
    e:(flattenLocalBinds b ++ flattenExpr w)
flattenExpr e@((L loc (HsIf _ _ c t el))) =
    e:(concatMap flattenExpr [c, t, el])
flattenExpr e@((L loc (OpApp _ c t el))) =
    e:(concatMap flattenExpr [c, t, el])
flattenExpr e = [e]

flattenLocalBinds :: LHsLocalBinds GhcPs -> [LHsExpr GhcPs]
flattenLocalBinds (L _ (HsValBinds _ (ValBinds _ bs _))) =
    flattenBinds  bs
flattenLocalBinds _ = []

flattenBinds :: LHsBinds GhcPs -> [LHsExpr GhcPs]
flattenBinds = concatMap flattenBind . bagToList

flattenBind :: LHsBindLR GhcPs GhcPs -> [LHsExpr GhcPs]
flattenBind (L _ (FunBind{fun_matches=MG{mg_alts =(L _ mtcs)}})) =
    concatMap flattenMatch mtcs
flattenBind _ = []

flattenMatch :: (LMatch GhcPs (LHsExpr GhcPs)) -> [LHsExpr GhcPs]
flattenMatch (L _ (Match{m_grhss=m_grhss})) = flattenGRHSs m_grhss
flattenMatch _ = []

flattenGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> [LHsExpr GhcPs ]
flattenGRHSs (grhss@GRHSs{..})  =
    (concatMap flattenLGRHS grhssGRHSs) ++ flattenLocalBinds grhssLocalBinds
flattenGRHSs _ = []

flattenLGRHS :: LGRHS GhcPs (LHsExpr GhcPs)  -> [LHsExpr GhcPs]
flattenLGRHS (L _ (GRHS _ guards e)) =
    concatMap flattenGStmt guards ++ flattenExpr e

flattenGStmt :: GuardLStmt GhcPs  -> [LHsExpr GhcPs]
flattenGStmt = flattenLStmt

-- TODO: More statements
flattenLStmt :: LStmt GhcPs (LHsExpr GhcPs) -> [LHsExpr GhcPs]
flattenLStmt (L l (LastStmt _ e _ _)) = flattenExpr e
flattenLStmt (L l (BodyStmt _ e _ _)) = flattenExpr e
flattenLStmt (L l (BindStmt _ _ e _ _)) = flattenExpr e
flattenLStmt (L l (LetStmt _ lbs)) = flattenLocalBinds lbs
flattenLStmt e = error (showSDocUnsafe $ ppr e)