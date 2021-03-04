module Synth.Sanctify where

import GhcPlugins
import OccName
import GHC
import Bag

import Synth.Util
import Synth.Types

-- The baseHole, available anywhere you could put an expression
baseHole :: SrcSpan -> (SrcSpan, LHsExpr GhcPs)
baseHole loc =
  (loc, L loc (HsUnboundVar noExtField (TrueExprHole (mkVarOcc "_"))))

-- All possible replacement of one variable with a hole, i.e. we are making
-- the expression "holey". Could also be named `perforate`, `stigmatize` or
-- `spindle`. See https://twitter.com/tritlo/status/1367202546415206400
sanctifyExpr :: LHsExpr GhcPs -> [(SrcSpan, LHsExpr GhcPs)]
-- The first one is the only time we do anything, the rest are just for
-- recursing through the structure of the expression.
sanctifyExpr (L loc (HsVar x (L _ v))) =
    [(loc, L loc (HsUnboundVar x (TrueExprHole name)))]
  where (ns,fs) = (occNameSpace (occName v), occNameFS (occName v))
        name = mkOccNameFS ns (concatFS $ (fsLit "_"):[fs])
sanctifyExpr (L loc (HsApp x l r)) = (baseHole loc):(rl ++ rr)
  where rl = map (\(l',e)-> (l', L loc (HsApp x e r))) $ sanctifyExpr l
        rr = map (\(l',e)-> (l', L loc (HsApp x l e))) $ sanctifyExpr r
sanctifyExpr (L loc (HsPar x e)) = ((baseHole loc) :) $
    map (\(l',e') -> (l', L loc (HsPar x e'))) $ sanctifyExpr e
sanctifyExpr (L loc (ExprWithTySig x e t)) = ((baseHole loc) :) $
    map (\(l',e') -> (l', L loc (ExprWithTySig x e' t))) $ sanctifyExpr e
sanctifyExpr (L loc (HsLet x b e)) = ((baseHole loc) :) $
       (map (\(l',b') -> (l', L loc $ HsLet x b' e)) $ sanctifyLocalBinds b)
   ++  (map (\(l',e') -> (l', L loc $ HsLet x b e')) $ sanctifyExpr e)
sanctifyExpr ((L loc (HsIf x se c t e))) = ((baseHole loc) :) $
      (map (\(l',c') -> (l', L loc (HsIf x se c' t e))) $ sanctifyExpr c)
   ++ (map (\(l',t') -> (l', L loc (HsIf x se c t' e))) $ sanctifyExpr t)
   ++ (map (\(l',e') -> (l', L loc (HsIf x se c t e'))) $ sanctifyExpr e)
sanctifyExpr ((L loc (OpApp x c t e))) = ((baseHole loc) :) $
      (map (\(l',c') -> (l', L loc (OpApp x c' t e))) $ sanctifyExpr c)
   ++ (map (\(l',t') -> (l', L loc (OpApp x c t' e))) $ sanctifyExpr t)
   ++ (map (\(l',e') -> (l', L loc (OpApp x c t e'))) $ sanctifyExpr e)
sanctifyExpr (L l _) = [baseHole l]


sanctifyLocalBinds :: LHsLocalBinds GhcPs -> [(SrcSpan, LHsLocalBinds GhcPs)]
sanctifyLocalBinds (L loc (HsValBinds x (ValBinds x2 b sigs ))) =
   map (\(l',b') -> (l', (L loc (HsValBinds x (ValBinds x2 b' sigs))))) $ sanctifyBinds b
sanctifyLocalBinds _ = []

sanctifyBinds :: LHsBinds GhcPs -> [(SrcSpan, LHsBinds GhcPs)]
sanctifyBinds = concatMap sanctifyOne . oneAndRest . bagToList
  where sanctifyOne (b,i,r) =
            map (\(l,b') -> (l, listToBag (insertAt i b' r))) $ sanctifyBind b

sanctifyBind :: LHsBind GhcPs -> [(SrcSpan, LHsBind GhcPs)]
sanctifyBind (L loc (fb@FunBind{fun_matches=mg@MG{mg_alts=(L locms mtcs)}})) =
  (map (\(l,m') -> (l,(L loc $ fb {fun_matches = mg {mg_alts=(L locms m')}})))
  . applToEach sanctifyMatch)  mtcs
sanctifyBind _ = []

sanctifyMatch :: LMatch GhcPs (LHsExpr GhcPs) -> [(SrcSpan, LMatch GhcPs (LHsExpr GhcPs))]
sanctifyMatch (L loc (m@Match{m_grhss=m_grhss})) =
  map (\(l,g') -> (l, (L loc (m{m_grhss=g'})))) $ sanctifyGRHSs m_grhss
sanctifyMatch _ = []

sanctifyGRHSs :: GRHSs GhcPs (LHsExpr GhcPs) -> [(SrcSpan, GRHSs GhcPs (LHsExpr GhcPs))]
sanctifyGRHSs (GRHSs x grhss lbs) =
  (map (\(l,g') -> (l, GRHSs x g' lbs)) $ applToEach sanctifyGRHS grhss)
  ++ slb
  where slb = map (\(l, lb') -> (l, GRHSs x grhss lb')) $ sanctifyLocalBinds lbs
sanctifyGRHSs _ = []

sanctifyGRHS :: LGRHS GhcPs (LHsExpr GhcPs) -> [(SrcSpan, LGRHS GhcPs (LHsExpr GhcPs))]
sanctifyGRHS (L l (GRHS x guards e)) =
  (map (\(l',g') -> (l', L l (GRHS x g' e))) $ applToEach sanctifyGStmt guards)
  ++ (map (\(l',e') -> (l', L l (GRHS x guards e'))) $ sanctifyExpr e)
  -- We've seen this pattern in bind, match and grhss, we should abstract it.
sanctifyGRHS _ = []

sanctifyGStmt :: GuardLStmt GhcPs -> [(SrcSpan, GuardLStmt GhcPs)]
sanctifyGStmt = sanctifyLStmt

sanctifyLStmt :: LStmt GhcPs (LHsExpr GhcPs) -> [(SrcSpan, LStmt GhcPs (LHsExpr GhcPs))]
sanctifyLStmt (L l (LastStmt x e b s))
    = map (\(l',e') -> (l', L l (LastStmt x e' b s))) $ sanctifyExpr e
sanctifyLStmt (L l (BindStmt x p e se1 se2))
    = map (\(l',e') -> (l',L l (BindStmt x p e' se1 se2))) $ sanctifyExpr e
sanctifyLStmt (L l (BodyStmt x e se1 se2))
    = map (\(l',e') -> (l', L l (BodyStmt x e' se1 se2))) $ sanctifyExpr e
sanctifyLStmt (L l (LetStmt x lbs)) =
  map (\(l',lb') -> (l',L l (LetStmt x lb'))) $ sanctifyLocalBinds lbs
sanctifyLStmt _ = []