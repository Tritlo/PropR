{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : PropR.Check
-- Description : Interfaces the Program and its variants towards QuickCheck.
-- License     : MIT
-- Stability   : experimental
--
-- This module handles calls and configurations of QuickCheck.
-- It only builds the checks - it does not execute them.
-- This module is a pure module.
module PropR.Check where

import Bag (emptyBag, listToBag, unionManyBags, unitBag)
import BasicTypes (IntegralLit (..), Origin (..), PromotionFlag (..), SourceText (..))
import Control.Lens (universeOnOf)
import Control.Lens.Extras (uniplate)
import Data.Data.Lens (tinplate)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShow)
import FastString (fsLit)
import GHC
import GhcPlugins (Outputable (ppr), occName, showSDocUnsafe)
import OccName (NameSpace, dataName, mkVarOcc, occNameString, tcName)
import PropR.Configuration (CompileConfig (..))
import PropR.Types (EExpr, EProblem (..), EProg, EProgFix, EProp)
import PropR.Util (progAtTy, propVars, propFunArgVars, rdrNamePrint, rdrNameToStr)
import RdrName (mkUnqual, mkVarUnqual, rdrNameOcc)
import TcEvidence (idHsWrapper)

data QcConfig = QcConfig {maxShrinks :: Maybe Int, maxSuccess :: Maybe Int, seed :: Int}

defaultQcConfig :: Int -> Int -> QcConfig
defaultQcConfig checks = QcConfig Nothing (Just checks)

-- | Manual HsExpr for `stdArgs { chatty = False, maxShrinks = 0}`
qcArgsExpr :: QcConfig -> LHsExpr GhcPs
qcArgsExpr QcConfig {..}
  | Just shrinks <- maxShrinks,
    Just successes <- maxSuccess =
    wrap2 "qcCheckArgsTestsMaxSeed" successes shrinks
  | Just shrinks <- maxShrinks = wrap1 "qcCheckArgsMaxSeed" shrinks
  | Just successes <- maxSuccess = wrap1 "qcCheckArgsTestsSeed" successes
  | otherwise = wrap (tf "qcCheckArgsSeed")
  where
    wrap wrapped = noLoc $ HsPar NoExtField $ noLoc $ HsApp NoExtField wrapped (il $ fromIntegral seed)
    wrap1 funName arg = wrap (noLoc $ HsPar NoExtField $ noLoc $ HsApp NoExtField (tf funName) (il $ fromIntegral arg))
    wrap2 funName arg1 arg2 =
      wrap
        ( noLoc $
            HsPar NoExtField $
              noLoc $
                HsApp NoExtField (noLoc $ HsPar NoExtField $ noLoc $ HsApp NoExtField (tf funName) (il $ fromIntegral arg1)) (il $ fromIntegral arg2)
        )

-- [Note] We had a version with no seed, qcCheckArgsMax and qcCheckArgs, but those are deprecated.
-- the helper functions are still available in check-helpers.

-- | This imports are required for the program to run.
checkImports :: [String]
checkImports =
  [ "import Check.Helpers",
    "import System.Environment (getArgs)"
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

-- | Unpacks a function defined by baseFun
unFun :: HsBind GhcPs -> Maybe (LHsExpr GhcPs)
unFun (FunBind _ _ (MG _ ((L _ [L _ (Match _ _ _ (GRHSs _ [L _ (GRHS _ _ ex)] _))])) _) _ _) = Just ex
unFun _ = Nothing

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
  -- | A namespace to look for a function (datatype or function)
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
hole :: HsExpr GhcPs
hole = HsUnboundVar NoExtField (TrueExprHole $ mkVarOcc "_")

-- Note: Every fix is checked with the same seed, to make sure that
-- it's the fix that's making it work and not the seed.
buildFixCheck ::
  CompileConfig ->
  Int ->
  EProblem ->
  [EProgFix] ->
  (LHsLocalBinds GhcPs, LHsBind GhcPs)
buildFixCheck cc seed EProb {..} prog_fixes =
  (ctxt, check_bind)
  where
    (L bl (HsValBinds be (ValBinds vbe vbs vsigs))) = e_ctxt
    qcb = baseFun (mkVarUnqual $ fsLit "qc__") (qcArgsExpr $ (defaultQcConfig (qcChecks cc) seed) {maxShrinks = Just 0})
    nvbs =
      unionManyBags
        [ listToBag e_props,
          unitBag qcb,
          if isJust e_module then emptyBag else vbs
        ]
    ctxt =
      L bl $
        HsValBinds be $
          ValBinds vbe nvbs $
            if isJust e_module then e_prop_sigs else vsigs ++ e_prop_sigs

    testsToCheck = mapMaybe (testCheckExpr e_prog cc (tf "qcSuccess", tf "id")) e_props

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

-- | Creates the QuickCheck expresssion to be run. Takes in the name of the
-- function to use for extracting the result
testCheckExpr ::
  -- | The program we're checking
  EProg ->
  -- | The repair config
  CompileConfig ->
  -- | A compiled program that contains properties and everything to run them
  (LHsExpr GhcPs, LHsExpr GhcPs) ->
  -- | The property to check
  EProp ->
  -- | The resulting expression
  Maybe (LHsExpr GhcPs)
testCheckExpr e_prog CompConf {..} extractors prop
  | Just _ <- prop_to_name prop =
    Just $ noLoc $ HsApp NoExtField (noLoc $ HsApp NoExtField (tf "fmap") extractor) subExpr
  where
    prop_to_name :: LHsBind GhcPs -> Maybe (Located RdrName)
    prop_to_name (L _ FunBind {fun_id = fid}) = Just fid
    prop_to_name _ = Nothing

    prop_vars :: Set RdrName
    prop_vars = propVars prop

    prop_pats = propFunArgVars prop
    prop_name = fromJust $ prop_to_name prop

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
              (noLoc $ HsApp NoExtField (tf "checkTastyTree") (il timeout))
              app

    app =
      noLoc $
        HsPar NoExtField $ apps (reverse $ filter (\(n, _, _) -> n `Set.member` prop_vars) e_prog)
      where
        apps []
          -- if we get no argument and the property doesn't have any argument
          -- mentioned in the body of the function, we don't do anything
          | Set.null (prop_pats `Set.intersection` prop_vars) = noLoc $ HsVar NoExtField prop_name
          | otherwise = noLoc $ HsApp NoExtField (noLoc $ HsVar NoExtField prop_name) (tf "expr__")
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
                    (noLoc $ HsApp NoExtField (tf "qcWRes") (il timeout))
                    (tf "qc__")
              )
              app
testCheckExpr _ _ _ _ = Nothing

-- Gives the signature for the given prop,  with the end of the type modified by
-- trans (e.g. const (tt "Property"))
sigForProp :: [LSig GhcPs] -> (LHsType GhcPs -> LHsType GhcPs) -> EProp -> [LSig GhcPs]
sigForProp e_prop_sigs trans e_prop@(L _ FunBind {..}) =
  case filter isForProp e_prop_sigs of
    [L l (TypeSig e n (HsWC x (HsIB xb t)))] ->
      [L l (TypeSig e n $ HsWC x $ HsIB xb $ replLast t)]
    _ -> []
  where
    isForProp (L _ (TypeSig _ [L _ r] _)) | r == unLoc fun_id = True
    isForProp _ = False
    -- We add a within to the alternatives, so the type changes from Bool
    -- We make sure that we don't replace wildcard types.
    replLast :: LHsType GhcPs -> LHsType GhcPs
    replLast (L l (HsFunTy NoExtField k r)) =
      L l $ HsFunTy NoExtField k $ replLast r
    replLast t@(L _ (HsWildCardTy _)) = t
    replLast t = trans t
sigForProp _ _ _ = []

-- | The `buildCounterExampleExpr` functions creates an expression which when
-- evaluated returns an (Maybe [String]), where the result is a shrunk argument
-- to the given prop if it fails for the given program, and nothing otherwise.
-- Note that we have to have it take in a list of properties to match the shape
-- of bCEAT
buildCounterExampleCheck ::
  CompileConfig ->
  Int ->
  EProp ->
  EProblem ->
  (Int, LHsExpr GhcPs)
buildCounterExampleCheck
  cc@CompConf {..}
  seed
  prop@( L
           loc
           fb@FunBind
             { fun_matches = fm@MG {mg_alts = (L lm malts@(L _ Match {..} : _))},
               ..
             }
         )
  EProb {..} = (num_args, noLoc $ HsLet NoExtField ctxt check_prog)
    where
      (L bl (HsValBinds be vb)) = e_ctxt
      (ValBinds vbe vbs vsigs) = vb
      qcb = baseFun (mkVarUnqual $ fsLit "qc__") (qcArgsExpr $ defaultQcConfig qcChecks seed)
      nvb = ValBinds vbe nvbs $ vsigs ++ nty
      nvbs =
        unionManyBags
          [ unitBag qcb,
            vbs,
            unitBag propWithin,
            listToBag expr_bs
          ]
      pvs = propVars prop
      num_args = length m_pats - length expr_bs
      nty = sigForProp e_prop_sigs (const $ tt "Property") prop
      expr_bs =
        map
          ( \(nm, e_ty, e_prog) ->
              baseFun (mkVarUnqual $ fsLit $ "expr__" ++ rdrNamePrint nm) $
                progAtTy e_prog e_ty
          )
          $ filter
            -- We don't want to clog the file with exprs that aren't being used
            -- We need the empty case for non-module fixes.
            (\(nm, _, _) -> nm `Set.member` pvs || rdrNameToStr nm == "")
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
                    (noLoc $ HsApp NoExtField (tf "qcWithin") (il timeout))
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
                    fromJust $ testCheckExpr e_prog cc (tf "failureToMaybe", tf "id") prop
              )
              sq_ty
buildCounterExampleCheck _ _ _ _ = error "invalid counter-example format!"
