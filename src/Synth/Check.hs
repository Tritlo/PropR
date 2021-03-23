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
import BasicTypes (IntegralLit(..), SourceText(..), PromotionFlag (..), Origin(..))
import ConLike (ConLike(..))
import Data.Maybe
import TcEvidence (idHsWrapper)


qcArgs = "stdArgs { chatty = False, maxShrinks = 0}"
-- Manual HsExpr for `stdArgs { chatty = False, maxShrinks = 0}`
qcArgsExpr :: Maybe Integer -> LHsExpr GhcPs
qcArgsExpr shrinks = noLoc $ RecordUpd NoExtField
                              (noLoc $ HsVar NoExtField $ noLoc $
                                   mkVarUnqual $ fsLit "stdArgs")
                               (chatty:(case shrinks of
                                           Just s -> [maxShrinks s]
                                           Nothing -> []))
   where
     unambig n =  (noLoc $ Unambiguous NoExtField $
                           noLoc $ mkVarUnqual $ fsLit n)
     rupd str e = noLoc $ HsRecField (unambig str) (noLoc e) False
     chatty :: LHsRecUpdField GhcPs
     chatty = rupd "chatty" (HsConLikeOut NoExtField $ RealDataCon falseDataCon)
     maxShrinks :: Integer -> LHsRecUpdField GhcPs
     maxShrinks shrinks = rupd "maxShrinks"
        (HsLit NoExtField (HsInt NoExtField $ IL NoSourceText False shrinks))

qcTime = 1000000
checkImports = [ "import Test.QuickCheck" ]


baseFun :: RdrName -> LHsExpr GhcPs -> LHsBind GhcPs
baseFun nm val = noLoc $ FunBind NoExtField (noLoc nm)
                           (MG NoExtField
                              (noLoc [base_case])
                              Generated) idHsWrapper []
   where base_case = noLoc $ Match NoExtField (FunRhs (noLoc nm) Prefix NoSrcStrict) []
                               (GRHSs NoExtField [noLoc $ GRHS NoExtField [] val] elb)
         elb :: LHsLocalBinds GhcPs
         elb = noLoc $ EmptyLocalBinds NoExtField

buildSuccessCheck :: EProblem -> EExpr
buildSuccessCheck EProb {..} =
      noLoc $ HsLet NoExtField ctxt check_prog
  where (L bl (HsValBinds be (ValBinds vbe vbs vsigs))) = e_ctxt
        qcb = baseFun (mkVarUnqual $ fsLit "qc__") (qcArgsExpr $ Just 0)
        nvb = (ValBinds vbe nvbs vsigs)
        nvbs = unionManyBags [unitBag qcb, vbs, listToBag e_props,
                             listToBag [expr_b, pcb]]
        expr_b = baseFun (mkVarUnqual $ fsLit "expr__")
                         (noLoc $ ExprWithTySig NoExtField
                                     (noLoc $ HsPar NoExtField e_prog) e_ty)
        ctxt = (L bl (HsValBinds be nvb))
        prop_to_name :: LHsBind GhcPs -> Maybe (Located RdrName)
        prop_to_name (L _ (FunBind {fun_id = fid})) = Just fid
        prop_to_name _ = Nothing
        prop_names = mapMaybe prop_to_name e_props
        tf :: String -> LHsExpr GhcPs
        tf = noLoc . HsVar NoExtField . noLoc . mkVarUnqual . fsLit
        il :: Integer -> LHsExpr GhcPs
        il = noLoc . HsLit NoExtField . HsInt NoExtField . IL NoSourceText False
        propsToCheck = map (propCheckExpr $ tf "isSuccess") prop_names


        pcb = baseFun (mkVarUnqual $ fsLit "propsToCheck__")
                      (noLoc $ ExplicitList NoExtField Nothing propsToCheck)
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

-- Runs the check with QuickCheck. Takes in the name of the function to use for
-- extracting the result
propCheckExpr :: LHsExpr GhcPs -> Located RdrName -> LHsExpr GhcPs
propCheckExpr extractor prop =
  noLoc $ HsApp NoExtField
            (noLoc $ HsApp NoExtField (tf "fmap") extractor)
            (noLoc $ HsPar NoExtField $ noLoc $ HsApp NoExtField
                        (noLoc $ HsApp NoExtField (tf "quickCheckWithResult") (tf "qc__"))
                        (noLoc $ HsPar NoExtField $ noLoc $
                              HsApp NoExtField
                              (noLoc $ HsApp NoExtField (tf "within") (il qcTime))
                              (noLoc $ HsPar NoExtField $
                                noLoc $ HsApp NoExtField
                                        (noLoc $ HsVar NoExtField prop)
                                        (tf "expr__"))))
  where tf = noLoc . HsVar NoExtField . noLoc . mkVarUnqual . fsLit
        il = noLoc . HsLit NoExtField . HsInt NoExtField . IL NoSourceText False

-- The `buildCounterExampleExpr` functions creates an expression which when
-- evaluated returns an (Maybe [String]), where the result is a shrunk argument
-- to the given prop if it fails for the given program, and nothing otherwise.
-- Note that we have to have it take in a list of properties to match the shape
-- of bCEAT
buildCounterExampleCheck :: EProp -> EProblem -> LHsExpr GhcPs -- RExpr
buildCounterExampleCheck prop@(L loc fb@(FunBind {fun_id=fid,
                                                  fun_matches=fm@MG{mg_alts= (L lm malts)}})) EProb{..} =
      noLoc $ HsLet NoExtField ctxt check_prog
  where (L bl (HsValBinds be vb)) = e_ctxt
        (ValBinds vbe vbs vsigs) = vb
        qcb = baseFun (mkVarUnqual $ fsLit "qc__") (qcArgsExpr Nothing)
        nvb = (ValBinds vbe nvbs vsigs)
        nvbs = unionManyBags [unitBag qcb, vbs,
                              listToBag [propWithin, expr_b, failFun]]
        expr_b = baseFun (mkVarUnqual $ fsLit "expr__")
                         (noLoc $ ExprWithTySig NoExtField
                           (noLoc $ HsPar NoExtField e_prog) e_ty)
        ctxt = (L bl (HsValBinds be nvb))
        propWithin = L loc fb {fun_matches = fm {mg_alts = (L lm malts')}}

        malts' = map addWithin malts
        addWithin (L l m@Match{m_grhss = grhs@(GRHSs {grhssGRHSs = bs})})
             =L l m{m_grhss = grhs {grhssGRHSs = bs'}}
           where bs' = map aW bs
                 aW g@(L l (GRHS x h b)) = (L l (GRHS x h b'))
                   where b' = noLoc $ HsApp NoExtField
                                      (noLoc $ HsApp NoExtField (tf "within") (il qcTime))
                                      (noLoc $ HsPar NoExtField b)
                 aW g = g

        tf = noLoc . HsVar NoExtField . noLoc . mkVarUnqual . fsLit
        il = noLoc . HsLit NoExtField . HsInt NoExtField . IL NoSourceText False
        tt :: String -> LHsType GhcPs
        tt = noLoc . HsTyVar NoExtField NotPromoted . noLoc . mkVarUnqual . fsLit
        sq_ty :: LHsSigWcType GhcPs
        sq_ty = HsWC NoExtField $
                 HsIB NoExtField $ noLoc $
                  HsAppTy NoExtField
                          (tt "IO")
                          -- Maybe String
                          $ noLoc $ HsParTy NoExtField $
                          (noLoc $ HsAppTy NoExtField (tt "Maybe")
                                      (noLoc $ HsListTy NoExtField $ tt "String"))
        check_prog :: LHsExpr GhcPs
        check_prog = noLoc $ HsPar NoExtField $ noLoc $ ExprWithTySig NoExtField
                   (noLoc $ HsPar NoExtField
                    (noLoc $ HsPar NoExtField $ propCheckExpr (tf "failureToMaybe") fid)) sq_ty

        -- Fail fun
        unambig :: String -> Located (FieldOcc GhcPs)
        unambig n = (noLoc $ FieldOcc NoExtField $ noLoc $ mkVarUnqual $ fsLit n)
        ffid :: Located RdrName
        ffid = (noLoc $ mkVarUnqual $ fsLit "failureToMaybe")

        other_case = (noLoc $ Match NoExtField
                                            (FunRhs ffid Prefix NoSrcStrict)
                                            [noLoc $ WildPat NoExtField]
                                            (GRHSs NoExtField
                                                   [noLoc $ GRHS NoExtField [] nothing]
                                                   elb))
          where nothing = noLoc $ (HsConLikeOut NoExtField $ RealDataCon nothingDataCon)
        failure_case = (noLoc $ Match NoExtField
                                            (FunRhs ffid Prefix NoSrcStrict)
                                            [noLoc $ ConPatIn failure fcondets]
                                            (GRHSs NoExtField
                                                   [noLoc $ GRHS NoExtField []
                                                    $ noLoc $ HsApp NoExtField
                                                     (noLoc $ HsConLikeOut NoExtField
                                                            $ RealDataCon $ justDataCon)
                                                   svar
                                                   ]
                                                   elb))
          where svar :: LHsExpr GhcPs
                svar = noLoc $ HsVar NoExtField $ svarname
                svarname :: Located RdrName
                svarname = noLoc $ mkVarUnqual $ fsLit "s"
                failure :: Located RdrName
                failure = noLoc $ mkVarUnqual $ fsLit "Failure"
                failing_tc :: Located RdrName
                failing_tc = noLoc $ mkVarUnqual $ fsLit "failingTestCase"
                fcondets :: HsConPatDetails GhcPs
                fcondets = RecCon $ HsRecFields [
                             noLoc $ HsRecField (noLoc $ FieldOcc NoExtField failing_tc)
                              flpat False] Nothing
                flpat :: LPat GhcPs
                flpat = noLoc $ VarPat NoExtField svarname
        elb :: LHsLocalBinds GhcPs
        elb = noLoc $ EmptyLocalBinds NoExtField
        failFun :: LHsBind GhcPs
        failFun = noLoc $ FunBind NoExtField ffid
                              (MG NoExtField
                                  (noLoc [failure_case, other_case])
                                  Generated) idHsWrapper []

