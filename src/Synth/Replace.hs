{-# LANGUAGE RecordWildCards #-}

module Synth.Replace where

import Bag
import Data.Map hiding (map)
import GHC
import Synth.Util

-- Fill the first hole in the expression.
replaceExpr :: Map SrcSpan (HsExpr GhcPs) -> LHsExpr GhcPs -> LHsExpr GhcPs
replaceExpr repls (L loc _) | loc `member` repls = L loc (repls ! loc)
replaceExpr repls (L loc (HsApp x l r)) =
  L loc $ HsApp x (re l) (re r)
  where
    re = replaceExpr repls
replaceExpr repls (L loc (HsPar x l)) =
  L loc . HsPar x $ replaceExpr repls l
replaceExpr repls (L loc (ExprWithTySig x l t)) =
  L loc . flip (ExprWithTySig x) t $ replaceExpr repls l
replaceExpr repls (L loc (HsLet x b e)) =
  L loc $ HsLet x (replaceExprLocalBinds repls b) (replaceExpr repls e)
replaceExpr repls (L loc (HsIf x mb c t e)) =
  L loc $ HsIf x mb (re c) (re t) (re e)
  where
    re = replaceExpr repls
replaceExpr repls (L loc (OpApp x c t e)) =
  L loc $ OpApp x (re c) (re t) (re e)
  where
    re = replaceExpr repls
replaceExpr repls (L loc (ExplicitTuple x args b)) =
  L loc $ ExplicitTuple x (map reArg args) b
  where
    re = replaceExpr repls
    reArg (L l (Present x e)) = L l (Present x (re e))
    reArg a = a
replaceExpr _ e = e

replaceExprLocalBinds :: Map SrcSpan (HsExpr GhcPs) -> LHsLocalBinds GhcPs -> LHsLocalBinds GhcPs
replaceExprLocalBinds repls (L loc (HsValBinds x (ValBinds xv bs sigs))) =
  L loc $ HsValBinds x $ ValBinds xv (replaceExprBinds repls bs) sigs
replaceExprLocalBinds _ e = e

replaceExprBinds ::
  Map SrcSpan (HsExpr GhcPs) ->
  LHsBinds GhcPs ->
  LHsBinds GhcPs
replaceExprBinds repls bs =
  listToBag $ map (replaceExprBind repls) (bagToList bs)

replaceExprBind ::
  Map SrcSpan (HsExpr GhcPs) ->
  LHsBindLR GhcPs GhcPs ->
  LHsBindLR GhcPs GhcPs
replaceExprBind repls (L loc fb@FunBind {fun_matches = mg@MG {mg_alts = (L locms mtcs)}}) =
  L loc (fb {fun_matches = mg {mg_alts = L locms (map (replaceMatch repls) mtcs)}})
replaceExprBind repls (L loc (VarBind x b v k)) =
  L loc . flip (VarBind x b) k $ replaceExpr repls v
replaceExprBind _ e = e

replaceMatch ::
  Map SrcSpan (HsExpr GhcPs) ->
  LMatch GhcPs (LHsExpr GhcPs) ->
  LMatch GhcPs (LHsExpr GhcPs)
replaceMatch repls (L loc m@Match {m_grhss = m_grhss}) =
  L loc (m {m_grhss = replaceGRHSs repls m_grhss})
replaceMatch _ e = e

replaceGRHSs ::
  Map SrcSpan (HsExpr GhcPs) ->
  GRHSs GhcPs (LHsExpr GhcPs) ->
  GRHSs GhcPs (LHsExpr GhcPs)
replaceGRHSs repls grhss@GRHSs {..} =
  grhss
    { grhssGRHSs = map (replaceLGRHS repls) grhssGRHSs,
      grhssLocalBinds = replaceExprLocalBinds repls grhssLocalBinds
    }
replaceGRHSs _ e = e

replaceLGRHS ::
  Map SrcSpan (HsExpr GhcPs) ->
  LGRHS GhcPs (LHsExpr GhcPs) ->
  LGRHS GhcPs (LHsExpr GhcPs)
replaceLGRHS repls (L l (GRHS x guards e)) =
  L l $ GRHS x (map (replaceGStmt repls) guards) (replaceExpr repls e)
replaceLGRHS _ e = e

replaceGStmt ::
  Map SrcSpan (HsExpr GhcPs) ->
  GuardLStmt GhcPs ->
  GuardLStmt GhcPs
replaceGStmt = replaceLStmt

-- TODO: more kinds of statements
replaceLStmt ::
  Map SrcSpan (HsExpr GhcPs) ->
  LStmt GhcPs (LHsExpr GhcPs) ->
  LStmt GhcPs (LHsExpr GhcPs)
replaceLStmt repls (L l (LastStmt x e b s)) =
  (\ne -> L l (LastStmt x ne b s)) $ replaceExpr repls e
replaceLStmt repls (L l (BodyStmt x e se1 se2)) =
  (\ne -> L l (BodyStmt x ne se1 se2)) $ replaceExpr repls e
-- Guard statements are Bind statements
replaceLStmt repls (L l (BindStmt x p e se1 se2)) =
  (\ne -> L l (BindStmt x p ne se1 se2)) $ replaceExpr repls e
replaceLStmt repls (L l (LetStmt x lbs)) =
  L l . LetStmt x $ replaceExprLocalBinds repls lbs
replaceLStmt _ e = e