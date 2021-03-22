{-# LANGUAGE RecordWildCards, TypeFamilies #-}
module Synth.Check where

import Synth.Util
import Synth.Types

import Data.List (intercalate)

import Bag
import GHC
import RdrName
import FastString
import TysWiredIn
import BasicTypes (IntegralLit(..), SourceText(..), PromotionFlag (..))
import ConLike (ConLike(..))
import Data.Maybe


qcArgs = "stdArgs { chatty = False, maxShrinks = 0}"
-- Manual HsExpr for `stdArgs { chatty = False, maxShrinks = 0}`
qcArgsExpr :: LHsExpr GhcPs
qcArgsExpr = noLoc $ RecordUpd NoExtField
                              (noLoc $ HsVar NoExtField $ noLoc $
                                   mkVarUnqual $ fsLit "stdArgs")
                              [chatty, maxShrinks]
   where
     unambig n =  (noLoc $ Unambiguous NoExtField $
                           noLoc $ mkVarUnqual $ fsLit n)
     rupd str e = noLoc $ HsRecField (unambig str) (noLoc e) False
     chatty :: LHsRecUpdField GhcPs
     chatty = rupd "chatty"
        (HsConLikeOut NoExtField $ RealDataCon falseDataCon)
     maxShrinks :: LHsRecUpdField GhcPs
     maxShrinks = rupd "maxShrinks"
        (HsLit NoExtField (HsInt NoExtField $ IL NoSourceText False 0))

qcTime = 1000000
checkImports = [ "import Test.QuickCheck" ]


buildProbCheck :: EProblem -> LHsExpr GhcPs -- RExpr
buildProbCheck EProb {..} =
      noLoc $ HsLet NoExtField ctxt check_prog
  where (L bl (HsValBinds be vb)) = e_ctxt
        (ValBinds vbe vbs vsigs) = vb
        qcb = noLoc (VarBind NoExtField (mkVarUnqual $ fsLit "qc__") qcArgsExpr False)
        nvb = (ValBinds vbe nvbs vsigs)
        nvbs = unionManyBags [unitBag qcb, vbs, listToBag e_props,
                             listToBag [expr_b, pcb]]
        expr_b = noLoc $ VarBind NoExtField
                              (mkVarUnqual $ fsLit "expr__")
                              (noLoc $ ExprWithTySig NoExtField
                                        (noLoc $ HsPar NoExtField e_prog) e_ty)
                              False
        ctxt = (L bl (HsValBinds be nvb))
        prop_to_name :: LHsBind GhcPs -> Maybe (Located RdrName)
        prop_to_name (L _ (FunBind {fun_id = fid})) = Just fid
        prop_to_name _ = Nothing
        prop_names = mapMaybe prop_to_name e_props
        tf = noLoc . HsVar NoExtField . noLoc . mkVarUnqual . fsLit
        il = noLoc . HsLit NoExtField . HsInt NoExtField . IL NoSourceText False
        propsToCheck = map propCheck prop_names


        propCheck :: Located RdrName -> LHsExpr GhcPs
        propCheck prop =
          noLoc $ HsApp NoExtField
                    (noLoc $ HsApp NoExtField (tf "fmap") (tf "isSuccess"))
                    (noLoc $ HsPar NoExtField $ noLoc $ HsApp NoExtField
                                (noLoc $ HsApp NoExtField (tf "quickCheckWithResult") (tf "qc__"))
                                (noLoc $ HsPar NoExtField $ noLoc $
                                     HsApp NoExtField
                                      (noLoc $ HsApp NoExtField (tf "within") (il qcTime))
                                      (noLoc $ HsPar NoExtField $
                                        noLoc $ HsApp NoExtField
                                                (noLoc $ HsVar NoExtField prop)
                                                (tf "expr__"))))
        pcb = noLoc $ VarBind NoExtField
                         (mkVarUnqual $ fsLit "propsToCheck__")
                         (noLoc $ ExplicitList NoExtField Nothing propsToCheck)
                         False
        tt :: String -> LHsType GhcPs
        tt = noLoc . HsTyVar NoExtField NotPromoted . noLoc . mkVarUnqual . fsLit
        sq_ty :: LHsSigWcType GhcPs
        sq_ty = HsWC NoExtField $
                 HsIB NoExtField $ noLoc $
                  HsAppTy NoExtField
                          (tt "IO")
                          (noLoc $ HsListTy NoExtField $ tt "Bool")
        check_prog :: LHsExpr GhcPs
        check_prog = noLoc $ HsPar NoExtField
         (noLoc $ ExprWithTySig NoExtField
                   (noLoc $ HsPar NoExtField
                    (noLoc $ HsApp NoExtField (tf "sequence")
                                              (tf "propsToCheck__"))) sq_ty)

-- Builds the actual check.
propCheck :: String -> String
propCheck pname = "quickCheckWithResult qc__ (within "
                ++ show qcTime ++ " (" ++ pname ++ " expr__ ))"

buildCheckExprAtTy :: RProblem -> RExpr
buildCheckExprAtTy RProb{..} =
     "let {" ++
       (intercalate "; " . concatMap lines $
         ("qc__ = " ++ qcArgs):r_ctxt
         ++ r_props
         ++ [ "expr__ :: " ++ r_ty
            , "expr__ = "++  r_prog
            , "propsToCheck__ = [ " ++
                 (intercalate ", " $ map propCheckExpr propNames) ++ "]" ])
     ++ "} in ((sequence propsToCheck__) :: IO [Bool])"
   where propNames = map (head . words) r_props
         -- We can't consolidate this into check__, since the type
         -- will be different!
         propCheckExpr pname = "isSuccess <$> " ++ propCheck pname


-- The `buildCounterExampleExpr` functions creates an expression which when
-- evaluated returns an (Maybe [String]), where the result is a shrunk argument
-- to the given prop if it fails for the given program, and nothing otherwise.
-- Note that we have to have it take in a list of properties to match the shape
-- of bCEAT
buildCounterExampleExpr :: RProp -> RProblem -> RExpr
buildCounterExampleExpr prop RProb{..} =
     "let {" ++
       (intercalate "; " . concatMap lines $
           ("qc__ = "  ++ qcArgs):r_ctxt
           ++ [ addWithin prop
              , "expr__ :: " ++ r_ty
              , "expr__ = "++  r_prog
              , "failureToMaybe :: Result -> Maybe [String]"
              , "failureToMaybe (Failure {failingTestCase = s}) = Just s"
              , "failureToMaybe _ = Nothing"
              , "propToCheck__ = failureToMaybe <$> " ++ propCheck propName])
      ++ "} in (propToCheck__) :: IO (Maybe [String])"
   where propName = head $ words prop
         -- We can't consolidate this into check__, since the type
         -- will be different!
         qcArgs = "stdArgs { chatty = False }"
         -- We have to have the within within the prop, otherwise we
         -- don't get the arguments used.
         addWithin prop = s ++ "= within " ++ (show qcTime) ++"(" ++ e ++ ")"
           where (s,('=':e)) = break ((==) '=') prop