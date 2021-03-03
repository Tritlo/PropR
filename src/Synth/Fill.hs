{-# LANGUAGE RecordWildCards #-}
module Synth.Fill where

import GHC
import Bag

import Synth.Util


-- Fill the first hole in the expression.
fillHole :: LHsExpr GhcPs -> HsExpr GhcPs -> Maybe (LHsExpr GhcPs)
fillHole (L loc (HsApp x l r)) fit
    = case fillHole l fit of
        Just res -> Just (L loc (HsApp x res r))
        Nothing -> case fillHole r fit of
                        Just res -> Just (L loc (HsApp x l res))
                        Nothing -> Nothing
fillHole (L loc (HsUnboundVar x _)) fit = Just (L loc fit)
fillHole (L loc (HsPar x l)) fit =
    fmap (L loc . HsPar x) $ fillHole l fit
fillHole (L loc (ExprWithTySig x l t)) fit =
    fmap (L loc . flip (ExprWithTySig x) t) $ fillHole l fit
fillHole (L loc (HsLet x b e)) fit =
    case fillHoleLocalBinds b fit of
        Just b' -> Just (L loc (HsLet x b' e))
        _ -> fmap (L loc . HsLet x b) $ fillHole e fit
fillHole (L loc (HsIf x mb c t e)) fit =
    case fillHole c fit of
      Just r -> Just (L loc (HsIf x mb r t e))
      _ -> case fillHole t fit of
             Just r -> Just (L loc (HsIf x mb c r e))
             _ -> case fillHole e fit of
                    Just r -> Just (L loc (HsIf x mb c t r))
                    Nothing -> Nothing
fillHole (L loc (OpApp x c t e)) fit =
    case fillHole c fit of
      Just r -> Just (L loc (OpApp x r t e))
      _ -> case fillHole t fit of
             Just r -> Just (L loc (OpApp x c r e))
             _ -> case fillHole e fit of
                    Just r -> Just (L loc (OpApp x c t r))
                    Nothing -> Nothing

fillHole e _ = Nothing

fillHoleLocalBinds :: LHsLocalBinds GhcPs -> HsExpr GhcPs -> Maybe (LHsLocalBinds GhcPs)
fillHoleLocalBinds (L loc (HsValBinds x (ValBinds xv bs sigs))) fit =
    fmap (\bs' -> (L loc (HsValBinds x (ValBinds xv bs' sigs)))) $ fillHoleBinds bs fit
fillHoleLocalBinds _ _ = Nothing

fillHoleBinds :: LHsBinds GhcPs
            -> HsExpr GhcPs
            -> Maybe (LHsBinds GhcPs)
fillHoleBinds bs fit =
     fmap listToBag $ mapFirst ((flip fillHoleBind) fit) (bagToList bs)

fillHoleBind :: LHsBindLR GhcPs GhcPs -> HsExpr GhcPs -> Maybe (LHsBindLR GhcPs GhcPs)
fillHoleBind (L loc (fb@FunBind{fun_matches=mg@MG{mg_alts =(L locms mtcs)}})) fit =
    case mapFirst ((flip fillMatch) fit) mtcs of
        Just r -> Just (L loc (fb {fun_matches=mg{mg_alts=(L locms r)}}))
        _ -> Nothing
fillHoleBind _ _ = Nothing

fillMatch :: (LMatch GhcPs (LHsExpr GhcPs))
          -> HsExpr GhcPs
          -> Maybe (LMatch GhcPs (LHsExpr GhcPs))
fillMatch (L loc (m@Match{m_grhss=m_grhss})) fit =
    case fillGRHSs m_grhss fit of
        Just r -> Just (L loc (m{m_grhss = r}))
        _ -> Nothing
fillMatch _ _ = Nothing

fillGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> HsExpr GhcPs -> Maybe (GRHSs GhcPs (LHsExpr GhcPs))
fillGRHSs (grhss@GRHSs{..}) fit =
    case mapFirst ((flip fillLGRHS) fit) grhssGRHSs of
        Just res -> Just (grhss {grhssGRHSs = res})
        _ -> case fillHoleLocalBinds grhssLocalBinds fit of
            Just lbs -> Just (grhss {grhssLocalBinds = lbs})
            _ -> Nothing
fillGRHSs _ _ = Nothing

fillLGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> HsExpr GhcPs -> Maybe (LGRHS GhcPs (LHsExpr GhcPs))
fillLGRHS (L l (GRHS x guards e)) fit =
    case mapFirst ((flip fillGStmt) fit) guards of
        Just res -> Just (L l (GRHS x res e))
        _ -> fmap (L l . GRHS x guards) $ fillHole e fit
fillLGRHS _ _ = Nothing

fillGStmt :: GuardLStmt GhcPs -> HsExpr GhcPs -> Maybe (GuardLStmt GhcPs)
fillGStmt e f = fillLStmt e f

-- TODO: more kinds of statements
fillLStmt :: LStmt GhcPs (LHsExpr GhcPs) -> HsExpr GhcPs -> Maybe (LStmt GhcPs (LHsExpr GhcPs))
fillLStmt (L l (LastStmt x e b s)) fit =
    fmap (\ne -> (L l (LastStmt x ne b s))) $ fillHole e fit
fillLStmt (L l (BodyStmt x e se1 se2)) fit =
    fmap (\ne -> (L l (BodyStmt x ne se1 se1))) $ fillHole e fit
-- Guard statements are Bind statements
fillLStmt (L l (BindStmt x p e se1 se2)) fit =
    fmap (\ne -> (L l (BindStmt x p ne se1 se1))) $ fillHole e fit
fillLStmt (L l (LetStmt x lbs)) fit =
    fmap (\ne -> (L l (LetStmt x lbs))) $ fillHoleLocalBinds lbs fit
fillLStmt _ _ = Nothing


fillHoles :: LHsExpr GhcPs -> [HsExpr GhcPs] -> Maybe (LHsExpr GhcPs)
fillHoles expr [] = Nothing
fillHoles expr (f:fs) = (fillHole expr f) >>= flip fillHoles fs