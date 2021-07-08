{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Endemic.Check
-- Description : Interfaces the Program and its variants towards QuickCheck.
-- License     : MIT
-- Stability   : experimental
--
-- This module handles calls and configurations of QuickCheck.
-- It only builds the checks - it does not execute them.
-- This module is a pure module.
module Endemic.Check where

import Bag
import BasicTypes (IntegralLit (..), Origin (..), PromotionFlag (..), SourceText (..))
import Data.List (intercalate)
import Data.Maybe
import FastString
import GHC
import OccName (NameSpace, dataName, mkVarOcc, tcName)
import RdrName
import Endemic.Types
import Endemic.Util
import TcEvidence (idHsWrapper)
import TysWiredIn

-- TODO: Give a seed for reproducible experiments & tests.

-- ^ Since QuickCheck doesn't support a simple integer seed, we'll have to
-- manually create a "QCGen" instance below to have a fixed seed.

-- | Manual HsExpr for `stdArgs { chatty = False, maxShrinks = 0}`
qcArgsExpr :: Maybe Integer -> LHsExpr GhcPs
qcArgsExpr shrinks =
  noLoc $
    RecordUpd
      { rupd_ext = NoExtField,
        rupd_expr = stdargs,
        rupd_flds = upds
      }
  where
    stdargs = noLoc $ HsVar NoExtField $ noLoc $ mkVarUnqual $ fsLit "stdArgs"
    upds =
      chatty :
      ( case shrinks of
          Just s -> [maxShrinks s]
          Nothing -> []
      )
    unambig n = noLoc $ Unambiguous NoExtField $ noLoc $ mkVarUnqual $ fsLit n
    rupd str e = noLoc $ HsRecField (unambig str) (noLoc e) False
    chatty :: LHsRecUpdField GhcPs
    chatty = rupd "chatty" (unLoc $ tfn dataName "False")
    maxShrinks :: Integer -> LHsRecUpdField GhcPs
    maxShrinks shrinks =
      rupd
        "maxShrinks"
        (HsLit NoExtField (HsInt NoExtField $ IL NoSourceText False shrinks))

-- | Time to run the QuickCheck in seconds
qcTime :: Integer
qcTime = 1_000_000

-- | This imports are required for the program to run.
checkImports = ["import Test.QuickCheck", "import System.Environment (getArgs)"]

-- | Looks up the given Name in a LHsExpr
baseFun :: RdrName -> LHsExpr GhcPs -> LHsBind GhcPs
baseFun nm val =
  noLoc $ FunBind NoExtField (noLoc nm) (MG NoExtField (noLoc [base_case]) Generated) idHsWrapper []
  where
    base_case =
      noLoc $
        Match
          NoExtField
          (FunRhs (noLoc nm) Prefix NoSrcStrict)
          []
          (GRHSs NoExtField [noLoc $ GRHS NoExtField [] val] elb)
    -- elb = empty local binds
    elb :: LHsLocalBinds GhcPs
    elb = noLoc $ EmptyLocalBinds NoExtField

-- Shorthands for common constructs

-- | Short for "the function"
tf ::
  -- | The string to lookup
  String ->
  -- | The matching function + location
  LHsExpr GhcPs
tf = noLoc . HsVar NoExtField . noLoc . mkVarUnqual . fsLit

-- | Runs tf in a given specified namespace
tfn ::
  -- | A namespace to look for a function ("Variable Scope", for non Haskellers)
  NameSpace ->
  -- | The name to look up
  String ->
  -- | The function that was searched for
  LHsExpr GhcPs
tfn ns = noLoc . HsVar NoExtField . noLoc . mkUnqual ns . fsLit

il :: Integer -> LHsExpr GhcPs
il = noLoc . HsLit NoExtField . HsInt NoExtField . IL NoSourceText False

-- | Short for "the type"
tt :: String -> LHsType GhcPs
tt = noLoc . HsTyVar NoExtField NotPromoted . noLoc . mkUnqual tcName . fsLit

-- | The building brick that resembles a "hole" for an expression.
hole :: LHsExpr GhcPs
hole = noLoc $ HsUnboundVar NoExtField (TrueExprHole $ mkVarOcc "_")

buildFixCheck :: EProblem -> [EExpr] -> (LHsLocalBinds GhcPs, LHsBind GhcPs)
buildFixCheck EProb {..} fixes =
  (ctxt, check_bind)
  where
    (L bl (HsValBinds be (ValBinds vbe vbs vsigs))) = e_ctxt
    qcb = baseFun (mkVarUnqual $ fsLit "qc__") (qcArgsExpr $ Just 0)
    nvb = ValBinds vbe nvbs vsigs
    nvbs =
      unionManyBags
        [ vbs,
          listToBag e_props,
          unitBag qcb
          -- unitBag expr_b,
          -- unitBag pcb
        ]
    ctxt = L bl (HsValBinds be nvb)
    prop_to_name :: LHsBind GhcPs -> Maybe (Located RdrName)
    prop_to_name (L _ FunBind {fun_id = fid}) = Just fid
    prop_to_name _ = Nothing
    prop_names = mapMaybe prop_to_name e_props
    propsToCheck = map (propCheckExpr $ tf "isSuccess") prop_names

    expr_b ep = baseFun (mkVarUnqual $ fsLit "expr__") $ progAtTy ep e_ty
    check_progs =
      map (\e -> noLoc $ HsLet NoExtField (eToBs e) par_app_w_ty) fixes
      where
        eToBs fix = noLoc $ HsValBinds NoExtField ebs
          where
            ebs = ValBinds NoExtField (unitBag (expr_b fix)) []
        elpc = noLoc $ ExplicitList NoExtField Nothing propsToCheck
        app :: LHsExpr GhcPs
        app = noLoc $ HsPar NoExtField $ noLoc $ HsApp NoExtField (tf "sequence") elpc
        app_w_ty :: LHsExpr GhcPs
        app_w_ty = noLoc $ ExprWithTySig NoExtField app sq_ty
        par_app_w_ty :: LHsExpr GhcPs
        par_app_w_ty = noLoc $ HsPar NoExtField app_w_ty

    check_bind =
      baseFun (mkVarUnqual $ fsLit "checks__") $
        noLoc $ ExplicitList NoExtField Nothing check_progs
    -- sq_ty is short for "sequence type"
    sq_ty :: LHsSigWcType GhcPs
    sq_ty =
      HsWC NoExtField $
        HsIB NoExtField $
          noLoc $
            HsAppTy
              NoExtField
              (tt "IO")
              (noLoc $ HsListTy NoExtField $ tt "Bool")

buildSuccessCheck :: EProblem -> EExpr
buildSuccessCheck EProb {..} =
  noLoc $ HsLet NoExtField ctxt check_prog
  where
    (L bl (HsValBinds be (ValBinds vbe vbs vsigs))) = e_ctxt
    qcb = baseFun (mkVarUnqual $ fsLit "qc__") (qcArgsExpr $ Just 0)
    nvb = ValBinds vbe nvbs vsigs
    nvbs =
      unionManyBags
        [ vbs,
          listToBag e_props,
          unitBag qcb,
          unitBag expr_b,
          unitBag pcb
        ]
    expr_b = baseFun (mkVarUnqual $ fsLit "expr__") $ progAtTy e_prog e_ty
    ctxt = L bl (HsValBinds be nvb)
    prop_to_name :: LHsBind GhcPs -> Maybe (Located RdrName)
    prop_to_name (L _ FunBind {fun_id = fid}) = Just fid
    prop_to_name _ = Nothing
    prop_names = mapMaybe prop_to_name e_props
    propsToCheck = map (propCheckExpr $ tf "isSuccess") prop_names

    pcb =
      baseFun
        (mkVarUnqual $ fsLit "propsToCheck__")
        (noLoc $ ExplicitList NoExtField Nothing propsToCheck)
    -- sq_ty is short for "sequence type"
    sq_ty :: LHsSigWcType GhcPs
    sq_ty =
      HsWC NoExtField $
        HsIB NoExtField $
          noLoc $
            HsAppTy
              NoExtField
              (tt "IO")
              (noLoc $ HsListTy NoExtField $ tt "Bool")
    check_prog :: LHsExpr GhcPs
    check_prog =
      noLoc $
        HsPar
          NoExtField
          ( noLoc $
              ExprWithTySig
                NoExtField
                ( noLoc $
                    HsPar
                      NoExtField
                      ( noLoc $
                          HsApp
                            NoExtField
                            (tf "sequence")
                            (tf "propsToCheck__")
                      )
                )
                sq_ty
          )

-- | Runs the check with QuickCheck. Takes in the name of the function to use for
-- extracting the result
propCheckExpr ::
  -- | A compiled program that contains properties and everything to run them
  LHsExpr GhcPs ->
  -- | A reader containing the property to check
  Located RdrName ->
  -- | The resulting expression
  LHsExpr GhcPs
propCheckExpr extractor prop =
  noLoc $
    HsApp
      NoExtField
      (noLoc $ HsApp NoExtField (tf "fmap") extractor)
      ( noLoc $
          HsPar NoExtField $
            noLoc $
              HsApp
                NoExtField
                (noLoc $ HsApp NoExtField (tf "quickCheckWithResult") (tf "qc__"))
                ( noLoc $
                    HsPar NoExtField $
                      noLoc $
                        HsApp
                          NoExtField
                          (noLoc $ HsApp NoExtField (tf "within") (il qcTime))
                          ( noLoc $
                              HsPar NoExtField $
                                noLoc $
                                  HsApp
                                    NoExtField
                                    (noLoc $ HsVar NoExtField prop)
                                    (tf "expr__")
                          )
                )
      )
  where
    tf = noLoc . HsVar NoExtField . noLoc . mkVarUnqual . fsLit
    il = noLoc . HsLit NoExtField . HsInt NoExtField . IL NoSourceText False

-- | The `buildCounterExampleExpr` functions creates an expression which when
-- evaluated returns an (Maybe [String]), where the result is a shrunk argument
-- to the given prop if it fails for the given program, and nothing otherwise.
-- Note that we have to have it take in a list of properties to match the shape
-- of bCEAT
buildCounterExampleCheck :: EProp -> EProblem -> LHsExpr GhcPs -- RExpr
buildCounterExampleCheck
  ( L
      loc
      fb@FunBind
        { fun_id = fid,
          fun_matches = fm@MG {mg_alts = (L lm malts)}
        }
    )
  EProb {..} = noLoc $ HsLet NoExtField ctxt check_prog
    where
      (L bl (HsValBinds be vb)) = e_ctxt
      (ValBinds vbe vbs vsigs) = vb
      qcb = baseFun (mkVarUnqual $ fsLit "qc__") (qcArgsExpr Nothing)
      nvb = ValBinds vbe nvbs vsigs
      nvbs =
        unionManyBags
          [ unitBag qcb,
            vbs,
            listToBag [propWithin, expr_b, failFun]
          ]
      expr_b = baseFun (mkVarUnqual $ fsLit "expr__") $ progAtTy e_prog e_ty
      ctxt = L bl (HsValBinds be nvb)
      propWithin = L loc fb {fun_matches = fm {mg_alts = L lm malts'}}

      malts' = map addWithin malts
      addWithin (L l m@Match {m_grhss = grhs@GRHSs {grhssGRHSs = bs}}) =
        L l m {m_grhss = grhs {grhssGRHSs = bs'}}
        where
          bs' = map aW bs
          aW g@(L l (GRHS x h b)) = L l (GRHS x h b')
            where
              b' =
                noLoc $
                  HsApp
                    NoExtField
                    (noLoc $ HsApp NoExtField (tf "within") (il qcTime))
                    (noLoc $ HsPar NoExtField b)
          aW g = g

      sq_ty :: LHsSigWcType GhcPs
      sq_ty =
        HsWC NoExtField $
          HsIB NoExtField $
            noLoc $
              HsAppTy
                NoExtField
                (tt "IO")
                ( noLoc $
                    HsParTy NoExtField $
                      noLoc
                        ( HsAppTy
                            NoExtField
                            (tt "Maybe")
                            (noLoc $ HsListTy NoExtField $ tt "String")
                        )
                )
      check_prog :: LHsExpr GhcPs
      check_prog =
        noLoc $
          HsPar NoExtField $
            progAtTy
              ( noLoc $
                  HsPar NoExtField $
                    propCheckExpr (tf "failureToMaybe") fid
              )
              sq_ty

      ffid :: Located RdrName
      ffid = noLoc $ mkVarUnqual $ fsLit "failureToMaybe"

      other_case =
        noLoc $
          Match
            NoExtField
            (FunRhs ffid Prefix NoSrcStrict)
            [noLoc $ WildPat NoExtField]
            ( GRHSs
                NoExtField
                [noLoc $ GRHS NoExtField [] nothing]
                elb
            )
        where
          nothing = tfn dataName "Nothing"
      failure_case =
        noLoc $
          Match
            NoExtField
            (FunRhs ffid Prefix NoSrcStrict)
            [noLoc $ ConPatIn failure fcondets]
            ( GRHSs
                NoExtField
                [ noLoc $
                    GRHS NoExtField [] $
                      noLoc $
                        HsApp
                          NoExtField
                          (tfn dataName "Just")
                          svar
                ]
                elb
            )
        where
          svar :: LHsExpr GhcPs
          svar = noLoc $ HsVar NoExtField svarname
          svarname :: Located RdrName
          svarname = noLoc $ mkVarUnqual $ fsLit "s"
          failure :: Located RdrName
          failure = noLoc $ mkUnqual dataName $ fsLit "Failure"
          failing_tc :: Located RdrName
          failing_tc = noLoc $ mkVarUnqual $ fsLit "failingTestCase"
          fcondets :: HsConPatDetails GhcPs
          fcondets =
            RecCon $
              HsRecFields
                [ noLoc $
                    HsRecField
                      (noLoc $ FieldOcc NoExtField failing_tc)
                      flpat
                      False
                ]
                Nothing
          flpat :: LPat GhcPs
          flpat = noLoc $ VarPat NoExtField svarname
      -- elb = empty local binds
      elb :: LHsLocalBinds GhcPs
      elb = noLoc $ EmptyLocalBinds NoExtField
      failFun :: LHsBind GhcPs
      failFun =
        noLoc $
          FunBind
            NoExtField
            ffid
            ( MG
                NoExtField
                (noLoc [failure_case, other_case])
                Generated
            )
            idHsWrapper
            []
