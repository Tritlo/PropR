{-# LANGUAGE RecordWildCards #-}

module Synth.Fill where

import Bag
import GHC
import Synth.Util

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infixl 4 <$$>

-- Fill the first hole in the expression.
fillHole :: LHsExpr GhcPs -> HsExpr GhcPs -> Maybe (SrcSpan, LHsExpr GhcPs)
fillHole (L loc (HsApp x l r)) fit =
  case fillHole l fit of
    Just (rl, res) -> Just (rl, L loc (HsApp x res r))
    Nothing -> case fillHole r fit of
      Just (rl, res) -> Just (rl, L loc (HsApp x l res))
      Nothing -> Nothing
fillHole (L loc (HsUnboundVar x _)) fit = Just (loc, L loc fit)
fillHole (L loc (HsPar x l)) fit =
  L loc . HsPar x <$$> fillHole l fit
fillHole (L loc (ExprWithTySig x l t)) fit =
  L loc . flip (ExprWithTySig x) t <$$> fillHole l fit
fillHole (L loc (HsLet x b e)) fit =
  case fillHoleLocalBinds b fit of
    Just (bl, b') -> Just (bl, L loc (HsLet x b' e))
    _ -> L loc . HsLet x b <$$> fillHole e fit
fillHole (L loc (HsIf x mb c t e)) fit =
  case fillHole c fit of
    Just (rl, r) -> Just (rl, L loc (HsIf x mb r t e))
    _ -> case fillHole t fit of
      Just (rl, r) -> Just (rl, L loc (HsIf x mb c r e))
      _ -> case fillHole e fit of
        Just (rl, r) -> Just (rl, L loc (HsIf x mb c t r))
        Nothing -> Nothing
fillHole (L loc (OpApp x c t e)) fit =
  case fillHole c fit of
    Just (rl, r) -> Just (rl, L loc (OpApp x r t e))
    _ -> case fillHole t fit of
      Just (rl, r) -> Just (rl, L loc (OpApp x c r e))
      _ -> case fillHole e fit of
        Just (rl, r) -> Just (rl, L loc (OpApp x c t r))
        Nothing -> Nothing
fillHole e _ = Nothing

fillHoleLocalBinds :: LHsLocalBinds GhcPs -> HsExpr GhcPs -> Maybe (SrcSpan, LHsLocalBinds GhcPs)
fillHoleLocalBinds (L loc (HsValBinds x (ValBinds xv bs sigs))) fit =
  (\bs' -> L loc (HsValBinds x (ValBinds xv bs' sigs))) <$$> fillHoleBinds bs fit
fillHoleLocalBinds _ _ = Nothing

fillHoleBinds ::
  LHsBinds GhcPs ->
  HsExpr GhcPs ->
  Maybe (SrcSpan, LHsBinds GhcPs)
fillHoleBinds bs fit =
  listToBag <$$> mapFirst (`fillHoleBind` fit) (bagToList bs)

fillHoleBind :: LHsBindLR GhcPs GhcPs -> HsExpr GhcPs -> Maybe (SrcSpan, LHsBindLR GhcPs GhcPs)
fillHoleBind (L loc fb@FunBind {fun_matches = mg@MG {mg_alts = (L locms mtcs)}}) fit =
  case mapFirst (`fillMatch` fit) mtcs of
    Just (rl, r) -> Just (rl, L loc (fb {fun_matches = mg {mg_alts = L locms r}}))
    _ -> Nothing
fillHoleBind (L loc (VarBind x b v k)) fit =
  L loc . flip (VarBind x b) k <$$> fillHole v fit
fillHoleBind _ _ = Nothing

fillMatch ::
  LMatch GhcPs (LHsExpr GhcPs) ->
  HsExpr GhcPs ->
  Maybe (SrcSpan, LMatch GhcPs (LHsExpr GhcPs))
fillMatch (L loc m@Match {m_grhss = m_grhss}) fit =
  case fillGRHSs m_grhss fit of
    Just (rl, r) -> Just (rl, L loc (m {m_grhss = r}))
    _ -> Nothing
fillMatch _ _ = Nothing

fillGRHSs ::
  GRHSs GhcPs (LHsExpr GhcPs) ->
  HsExpr GhcPs ->
  Maybe (SrcSpan, GRHSs GhcPs (LHsExpr GhcPs))
fillGRHSs grhss@GRHSs {..} fit =
  case mapFirst (`fillLGRHS` fit) grhssGRHSs of
    Just (rl, res) -> Just (rl, grhss {grhssGRHSs = res})
    _ -> case fillHoleLocalBinds grhssLocalBinds fit of
      Just (rl, lbs) -> Just (rl, grhss {grhssLocalBinds = lbs})
      _ -> Nothing
fillGRHSs _ _ = Nothing

fillLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> HsExpr GhcPs -> Maybe (SrcSpan, LGRHS GhcPs (LHsExpr GhcPs))
fillLGRHS (L l (GRHS x guards e)) fit =
  case mapFirst (`fillGStmt` fit) guards of
    Just (rl, res) -> Just (rl, L l (GRHS x res e))
    _ -> L l . GRHS x guards <$$> fillHole e fit
fillLGRHS _ _ = Nothing

fillGStmt :: GuardLStmt GhcPs -> HsExpr GhcPs -> Maybe (SrcSpan, GuardLStmt GhcPs)
fillGStmt = fillLStmt

-- TODO: more kinds of statements
fillLStmt :: LStmt GhcPs (LHsExpr GhcPs) -> HsExpr GhcPs -> Maybe (SrcSpan, LStmt GhcPs (LHsExpr GhcPs))
fillLStmt (L l (LastStmt x e b s)) fit =
  (\ne -> L l (LastStmt x ne b s)) <$$> fillHole e fit
fillLStmt (L l (BodyStmt x e se1 se2)) fit =
  (\ne -> L l (BodyStmt x ne se1 se2)) <$$> fillHole e fit
-- Guard statements are Bind statements
fillLStmt (L l (BindStmt x p e se1 se2)) fit =
  (\ne -> L l (BindStmt x p ne se1 se2)) <$$> fillHole e fit
fillLStmt (L l (LetStmt x lbs)) fit =
  L l . LetStmt x <$$> fillHoleLocalBinds lbs fit
fillLStmt _ _ = Nothing