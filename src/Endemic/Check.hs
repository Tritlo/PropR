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

import Bag (emptyBag, listToBag, unionManyBags, unitBag)
import BasicTypes (IntegralLit (..), Origin (..), PromotionFlag (..), SourceText (..))
import Control.Lens (universeOnOf)
import Control.Lens.Extras (uniplate)
import Data.Data.Lens (tinplate)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic.Configuration (RepairConfig (..))
import Endemic.Types (EExpr, EProblem (..), EProg, EProgFix, EProp)
import Endemic.Util (progAtTy, propVars, rdrNamePrint)
import FastString (fsLit)
import GHC
import GhcPlugins (occName, ppr, showSDocUnsafe)
import OccName (NameSpace, dataName, mkVarOcc, occNameString, tcName)
import RdrName (mkUnqual, mkVarUnqual, rdrNameOcc)
import TcEvidence (idHsWrapper)

-- | Manual HsExpr for `stdArgs { chatty = False, maxShrinks = 0}`
qcArgsExpr :: Int -> Maybe Integer -> LHsExpr GhcPs
qcArgsExpr seed Nothing = noLoc $ HsPar NoExtField $ noLoc $ HsApp NoExtField (tf "qcCheckArgsSeed") (il $ fromIntegral seed)
qcArgsExpr seed (Just shrinks) =
  noLoc $
    HsPar NoExtField $
      noLoc $ HsApp NoExtField (noLoc $ HsPar NoExtField $ noLoc $ HsApp NoExtField (tf "qcCheckArgsMaxSeed") (il $ fromIntegral seed)) (il shrinks)

-- [Note] We had a version with no seed, qcCheckArgsMax and qcCheckArgs, but those are deprecated.
-- the helper functions are still available in check-helpers.

-- | This imports are required for the program to run.
checkImports :: [String]
checkImports =
  [ "import Check.Helpers",
    "import System.Environment (getArgs)",
    "import Prelude"
  ]

checkPackages :: [String]
checkPackages = ["base", "check-helpers"]

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
il = noLoc . HsPar NoExtField . noLoc . HsLit NoExtField . HsInt NoExtField . IL NoSourceText False

-- | Short for "the type"
tt :: String -> LHsType GhcPs
tt = noLoc . HsTyVar NoExtField NotPromoted . noLoc . mkUnqual tcName . fsLit

-- | The building brick that resembles a "hole" for an expression.
hole :: LHsExpr GhcPs
hole = noLoc $ HsUnboundVar NoExtField (TrueExprHole $ mkVarOcc "_")

-- Note: Every fix is checked with the same seed, to make sure that
-- it's the fix that's making it work and not the seed.
buildFixCheck ::
  RepairConfig ->
  Int ->
  EProblem ->
  [EProgFix] ->
  (LHsLocalBinds GhcPs, LHsBind GhcPs)
buildFixCheck rc seed EProb {..} prog_fixes =
  (ctxt, check_bind)
  where
    (L bl (HsValBinds be (ValBinds vbe vbs vsigs))) = e_ctxt
    qcb = baseFun (mkVarUnqual $ fsLit "qc__") (qcArgsExpr seed $ Just 0)
    nvbs =
      unionManyBags
        [ listToBag e_props,
          unitBag qcb,
          if isJust e_module then emptyBag else vbs
        ]
    ctxt = L bl (HsValBinds be (ValBinds vbe nvbs $ if (isJust e_module) then [] else vsigs))

    testsToCheck = mapMaybe (testCheckExpr e_prog rc (tf "qcSuccess", tf "id")) e_props

    expr_bs prog_fix =
      zipWith
        ( \(nm, e_ty, _) n_prog ->
            baseFun (mkVarUnqual $ fsLit $ "expr__" ++ rdrNamePrint nm) $
              progAtTy n_prog e_ty
        )
        e_prog
        prog_fix
    check_progs =
      map (\e -> noLoc $ HsLet NoExtField (eToBs e) par_app_w_ty) prog_fixes
      where
        eToBs fix = noLoc $ HsValBinds NoExtField ebs
          where
            ebs = ValBinds NoExtField (listToBag (expr_bs fix)) []
        elpc = noLoc $ ExplicitList NoExtField Nothing testsToCheck
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
buildFixCheck _ _ _ _ = error "External not supported!"

-- | Runs the check with QuickCheck. Takes in the name of the function to use for
-- extracting the result
testCheckExpr ::
  -- | The program we're checking
  EProg ->
  -- | The repair config
  RepairConfig ->
  -- | A compiled program that contains properties and everything to run them
  (LHsExpr GhcPs, LHsExpr GhcPs) ->
  -- | The property to check
  EProp ->
  -- | The resulting expression
  Maybe (LHsExpr GhcPs)
testCheckExpr e_prog RepConf {..} extractors prop
  | Just _ <- prop_to_name prop =
    Just $ noLoc $ HsApp NoExtField (noLoc $ HsApp NoExtField (tf "fmap") extractor) subExpr
  where
    prop_to_name :: LHsBind GhcPs -> Maybe (Located RdrName)
    prop_to_name (L _ FunBind {fun_id = fid}) = Just fid
    prop_to_name _ = Nothing

    prop_name = fromJust $ prop_to_name prop

    prop_vars :: Set RdrName
    prop_vars = propVars prop

    isQc = ((==) "prop" . take 4 . occNameString . rdrNameOcc . unLoc) prop_name
    extractor = if isQc then fst extractors else snd extractors
    subExpr :: LHsExpr GhcPs
    subExpr = if isQc then qcSubExpr else tastySubExpr
    tastySubExpr :: LHsExpr GhcPs
    tastySubExpr =
      noLoc $
        HsPar NoExtField $
          noLoc $
            HsApp
              NoExtField
              (noLoc $ HsApp NoExtField (tf "checkTastyTree") (il repTimeout))
              app

    app =
      noLoc $
        HsPar NoExtField $ apps (reverse $ filter (\(n, _, _) -> n `Set.member` prop_vars) e_prog)
      where
        -- If there's no var, then we have to do the following
        apps [] =
          noLoc $
            HsApp
              NoExtField
              (noLoc $ HsVar NoExtField prop_name)
              (tf "expr__")
        apps [(nm, _, _)] =
          noLoc $
            HsApp
              NoExtField
              (noLoc $ HsVar NoExtField prop_name)
              (tf ("expr__" ++ rdrNamePrint nm))
        apps ((nm, _, _) : r) =
          noLoc $ HsApp NoExtField (apps r) (tf $ "expr__" ++ rdrNamePrint nm)

    qcSubExpr :: LHsExpr GhcPs
    qcSubExpr =
      noLoc $
        HsPar NoExtField $
          noLoc $
            HsApp
              NoExtField
              ( noLoc $
                  HsApp
                    NoExtField
                    (noLoc $ HsApp NoExtField (tf "qcWRes") (il repTimeout))
                    (tf "qc__")
              )
              app
testCheckExpr _ _ _ _ = Nothing

-- | The `buildCounterExampleExpr` functions creates an expression which when
-- evaluated returns an (Maybe [String]), where the result is a shrunk argument
-- to the given prop if it fails for the given program, and nothing otherwise.
-- Note that we have to have it take in a list of properties to match the shape
-- of bCEAT
buildCounterExampleCheck ::
  RepairConfig ->
  Int ->
  EProp ->
  EProblem ->
  LHsExpr GhcPs
buildCounterExampleCheck
  rc@RepConf {..}
  seed
  prop@( L
           loc
           fb@FunBind
             { fun_matches = fm@MG {mg_alts = (L lm malts)}
             }
         )
  EProb {..} = noLoc $ HsLet NoExtField ctxt check_prog
    where
      (L bl (HsValBinds be vb)) = e_ctxt
      (ValBinds vbe vbs vsigs) = vb
      qcb = baseFun (mkVarUnqual $ fsLit "qc__") (qcArgsExpr seed Nothing)
      nvb = ValBinds vbe nvbs vsigs
      nvbs =
        unionManyBags
          [ unitBag qcb,
            vbs,
            unitBag propWithin,
            listToBag expr_bs
          ]

      expr_bs =
        map
          ( \(nm, e_ty, e_prog) ->
              baseFun (mkVarUnqual $ fsLit $ "expr__" ++ rdrNamePrint nm) $
                progAtTy e_prog e_ty
          )
          e_prog
      ctxt = L bl (HsValBinds be nvb)
      propWithin = L loc fb {fun_matches = fm {mg_alts = L lm malts'}}

      malts' = map addWithin malts
      addWithin (L l m@Match {m_grhss = grhs@GRHSs {grhssGRHSs = bs}}) =
        L l m {m_grhss = grhs {grhssGRHSs = bs'}}
        where
          bs' = map aW bs
          aW (L l' (GRHS x h b)) = L l' (GRHS x h b')
            where
              b' =
                noLoc $
                  HsApp
                    NoExtField
                    (noLoc $ HsApp NoExtField (tf "qcWithin") (il repTimeout))
                    (noLoc $ HsPar NoExtField b)
          aW g = g
      addWithin malt = malt

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
                    fromJust $ testCheckExpr e_prog rc (tf "failureToMaybe", tf "id") prop
              )
              sq_ty
buildCounterExampleCheck _ _ _ _ = error "invalid counter-example format!"
