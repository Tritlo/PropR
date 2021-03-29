{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Synth.Flatten where

import Bag
import GHC
import GhcPlugins (Outputable, ppr, showSDocUnsafe)
import Synth.Util

-- A naive implementation of flatten, works for very limited cases so far.
-- We'll need to walk over the entire Haskell AST eventually. We'd like for
-- this to maintain parity with Fill, but this is only used for tests currently.
flattenExpr :: OutputableBndrId pass => LHsExpr (GhcPass pass) -> [LHsExpr (GhcPass pass)]
flattenExpr e@(L loc (HsApp x l r)) = e : (flattenExpr l ++ flattenExpr r)
flattenExpr e@(L loc (HsPar x l)) =
  e : flattenExpr l
flattenExpr e@(L loc (ExprWithTySig x l t)) =
  e : flattenExpr l
flattenExpr e@((L loc (HsLet x b w))) =
  e : (flattenLocalBinds b ++ flattenExpr w)
flattenExpr e@((L loc (HsIf _ _ c t el))) =
  e : concatMap flattenExpr [c, t, el]
flattenExpr e@((L loc (OpApp _ c t el))) =
  e : concatMap flattenExpr [c, t, el]
flattenExpr e@(L loc (HsWrap x w l)) =
  e : flattenExpr (L loc l)
flattenExpr e = [e]

flattenLocalBinds :: OutputableBndrId pass => LHsLocalBinds (GhcPass pass) -> [LHsExpr (GhcPass pass)]
flattenLocalBinds (L _ (HsValBinds _ (ValBinds _ bs _))) = flattenBinds bs
flattenLocalBinds (L _ (HsValBinds _ (XValBindsLR (NValBinds rfbs _)))) =
  concatMap (flattenBinds . snd) rfbs
flattenLocalBinds _ = []

flattenBinds :: OutputableBndrId pass => LHsBinds (GhcPass pass) -> [LHsExpr (GhcPass pass)]
flattenBinds = concatMap flattenBind . bagToList

flattenBind :: OutputableBndrId pass => LHsBindLR (GhcPass pass) (GhcPass pass) -> [LHsExpr (GhcPass pass)]
flattenBind (L _ FunBind {fun_matches = MG {mg_alts = (L _ mtcs)}}) = concatMap flattenMatch mtcs
flattenBind (L _ b@AbsBinds {..}) = flattenBinds abs_binds
flattenBind (L _ b@VarBind {..}) = flattenExpr var_rhs
flattenBind (L _ b@PatBind {..}) = flattenGRHSs pat_rhs
flattenBind _ = []

flattenMatch :: OutputableBndrId pass => LMatch (GhcPass pass) (LHsExpr (GhcPass pass)) -> [LHsExpr (GhcPass pass)]
flattenMatch (L _ Match {m_grhss = m_grhss}) = flattenGRHSs m_grhss
flattenMatch _ = []

flattenGRHSs :: OutputableBndrId pass => GRHSs (GhcPass pass) (LHsExpr (GhcPass pass)) -> [LHsExpr (GhcPass pass)]
flattenGRHSs grhss@GRHSs {..} =
  concatMap flattenLGRHS grhssGRHSs ++ flattenLocalBinds grhssLocalBinds
flattenGRHSs _ = []

flattenLGRHS :: OutputableBndrId pass => LGRHS (GhcPass pass) (LHsExpr (GhcPass pass)) -> [LHsExpr (GhcPass pass)]
flattenLGRHS (L _ (GRHS _ guards e)) =
  concatMap flattenGStmt guards ++ flattenExpr e

flattenGStmt :: OutputableBndrId pass => GuardLStmt (GhcPass pass) -> [LHsExpr (GhcPass pass)]
flattenGStmt = flattenLStmt

-- TODO: More statements
flattenLStmt :: OutputableBndrId pass => LStmt (GhcPass pass) (LHsExpr (GhcPass pass)) -> [LHsExpr (GhcPass pass)]
flattenLStmt (L l (LastStmt _ e _ _)) = flattenExpr e
flattenLStmt (L l (BodyStmt _ e _ _)) = flattenExpr e
flattenLStmt (L l (BindStmt _ _ e _ _)) = flattenExpr e
flattenLStmt (L l (LetStmt _ lbs)) = flattenLocalBinds lbs
flattenLStmt e = error (showSDocUnsafe $ ppr e)