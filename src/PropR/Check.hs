{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

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

import Control.Lens (universeOnOf)
import Control.Lens.Extras (uniplate)
import Data.Data.Lens (tinplate)
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace (trace, traceShow)
import GHC.Data.FastString (fsLit)
import GHC
import GHC.Plugins (Outputable (ppr), occName, showSDocUnsafe, mkVarOcc, mkRdrUnqual)
import GHC.Data.Bag (emptyBag, listToBag, unionManyBags, unitBag)
import GHC.Types.Basic (Origin (..), PromotionFlag (..))
import GHC.Types.SourceText (IntegralLit (..), SourceText (..))
import GHC.Types.Name.Occurrence (NameSpace, dataName, mkVarOcc, occNameString, tcName)
import GHC.Types.Name.Reader (mkUnqual, mkVarUnqual, rdrNameOcc)
import GHC.Tc.Types.Evidence (idHsWrapper)
import PropR.Configuration (CompileConfig (..))
import PropR.Types (EExpr, EProblem (..), EProg, EProgFix, EProp)
import PropR.Util (progAtTy, propVars, propFunArgVars, rdrNamePrint, rdrNameToStr)

#if __GLASGOW_HASKELL__ >= 908
import GHC.Plugins (DoPmc(..))
#endif


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
    wrap wrapped = noLocA $
        HsPar noAnn noHsTok (noLocA $ HsApp noAnn wrapped (il $ fromIntegral seed)) noHsTok
    wrap1 funName arg = 
        wrap (noLocA $ HsPar noAnn noHsTok (noLocA $ HsApp noAnn (tf funName) (il $ fromIntegral arg)) noHsTok)
    wrap2 funName arg1 arg2 =
      wrap
        ( noLocA $
            HsPar noAnn noHsTok 
             (noLocA $
                HsApp noAnn 
                    (noLocA $ HsPar noAnn noHsTok
                              (noLocA $ HsApp noAnn (tf funName) (il $ fromIntegral arg1))
                              noHsTok)
                    (il $ fromIntegral arg2))
            noHsTok)

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
  noLocA $ FunBind NoExtField (noLocA nm)
    (MG
#if __GLASGOW_HASKELL__ >= 908
      (Generated DoPmc)
#else
      (Generated)
#endif
      (noLocA [base_case]))
  where
    base_case =
      noLocA $
        Match
          noAnn
          (FunRhs (noLocA nm) Prefix NoSrcStrict)
          []
          (GRHSs emptyComments [noLocA $ GRHS noAnn [] val] elb)
    -- elb = empty local binds
    elb :: HsLocalBinds GhcPs
    elb = EmptyLocalBinds NoExtField

-- | Unpacks a function defined by baseFun
unFun :: HsBind GhcPs -> Maybe (LHsExpr GhcPs)
unFun (FunBind _ _ (MG _ ((L _ [L _ (Match _ _ _ (GRHSs _ [L _ (GRHS _ _ ex)] _))])))) = Just ex
unFun _ = Nothing

-- Shorthands for common constructs

-- | Short for "the function"
tf ::
  -- | The string to lookup
  String ->
  -- | The matching function + location
  LHsExpr GhcPs
tf = noLocA . HsVar noExtField . noLocA . mkVarUnqual . fsLit

-- | Runs tf in a given specified namespace
tfn ::
  -- | A namespace to look for a function (datatype or function)
  NameSpace ->
  -- | The name to look up
  String ->
  -- | The function that was searched for
  LHsExpr GhcPs
tfn ns = noLocA . HsVar noExtField . noLocA . mkUnqual ns . fsLit

il :: Integer -> LHsExpr GhcPs
il i = noLocA $ 
        HsPar noAnn noHsTok 
            (noLocA $ HsLit noAnn $ HsInt noExtField $ IL NoSourceText False i)
            noHsTok

-- | Short for "the type"
tt :: String -> LHsType GhcPs
tt = noLocA . HsTyVar noAnn NotPromoted . noLocA . mkUnqual tcName . fsLit

-- | The building brick that resembles a "hole" for an expression.
hole :: HsExpr GhcPs
hole = HsUnboundVar noAnn (mkRdrUnqual $ mkVarOcc "_")

-- Note: Every fix is checked with the same seed, to make sure that
-- it's the fix that's making it work and not the seed.
buildFixCheck ::
  CompileConfig ->
  Int ->
  EProblem ->
  [EProgFix] ->
  (HsLocalBinds GhcPs, LHsBind GhcPs)
buildFixCheck cc seed EProb {..} prog_fixes =
  (ctxt, check_bind)
  where
    HsValBinds be (ValBinds vbe vbs vsigs) = e_ctxt
    qcb = baseFun (mkVarUnqual $ fsLit "qc__") (qcArgsExpr $ (defaultQcConfig (qcChecks cc) seed) {maxShrinks = Just 0})
    nvbs =
      unionManyBags
        [ listToBag e_props,
          unitBag qcb,
          if isJust e_module then emptyBag else vbs
        ]
    ctxt =
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
      map (\e -> noLocA $ HsLet noAnn noHsTok (eToBs e) noHsTok par_app_w_ty) prog_fixes
      where
        eToBs fix = HsValBinds noAnn ebs
          where
            ebs = ValBinds mempty (listToBag (expr_bs fix)) []
        elpc = noLocA $ ExplicitList noAnn testsToCheck
        app :: LHsExpr GhcPs
        app = noLocA $ HsPar noAnn noHsTok (noLocA $ HsApp noAnn (tf "sequence") elpc) noHsTok
        app_w_ty :: LHsExpr GhcPs
        app_w_ty = noLocA $ ExprWithTySig noAnn app sq_ty
        par_app_w_ty :: LHsExpr GhcPs
        par_app_w_ty = noLocA $ HsPar noAnn noHsTok app_w_ty noHsTok

    check_bind =
      baseFun (mkVarUnqual $ fsLit "checks__") $
        noLocA $ ExplicitList noAnn check_progs
    -- sq_ty is short for "sequence type"
    sq_ty :: LHsSigWcType GhcPs
    sq_ty =
      HsWC NoExtField $
        noLocA $
          HsSig NoExtField (HsOuterImplicit NoExtField) $
          noLocA $
            HsAppTy
              noExtField
              (tt "IO")
              (noLocA $ HsListTy noAnn $ tt "Bool")
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
    Just $ noLocA $ HsApp noAnn (noLocA $ HsApp noAnn (tf "fmap") extractor) subExpr
  where
    prop_to_name :: LHsBind GhcPs -> Maybe (LIdP GhcPs)
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
      noLocA $
        HsPar noAnn noHsTok 
          (noLocA $
            HsApp
              noAnn
              (noLocA $ HsApp noAnn (tf "checkTastyTree") (il timeout))
              app) noHsTok

    app =
      noLocA $
        HsPar noAnn noHsTok (apps (reverse $ filter (\(n, _, _) -> n `Set.member` prop_vars) e_prog)) noHsTok
      where
        apps []
          -- if we get no argument and the property doesn't have any argument
          -- mentioned in the body of the function, we don't do anything
          | Set.null (prop_pats `Set.intersection` prop_vars) = noLocA $ HsVar noExtField prop_name
          | otherwise = noLocA $ HsApp noAnn (noLocA $ HsVar noExtField prop_name) (tf "expr__")
        apps [(nm, _, _)] =
          noLocA $
            HsApp
              noAnn
              (noLocA $ HsVar noExtField prop_name)
              (tf ("expr__" ++ rdrNamePrint nm))
        apps ((nm, _, _) : r) =
          noLocA $ HsApp noAnn (apps r) (tf $ "expr__" ++ rdrNamePrint nm)

    qcSubExpr :: LHsExpr GhcPs
    qcSubExpr =
      noLocA $
        HsPar noAnn noHsTok 
          (noLocA $
            HsApp
              noAnn
              ( noLocA $
                  HsApp
                    noAnn
                    (noLocA $ HsApp noAnn (tf "qcWRes") (il timeout))
                    (tf "qc__")
              )
              app) noHsTok
testCheckExpr _ _ _ _ = Nothing

-- Gives the signature for the given prop,  with the end of the type modified by
-- trans (e.g. const (tt "Property"))
sigForProp :: [LSig GhcPs] -> (LHsType GhcPs -> LHsType GhcPs) -> EProp -> [LSig GhcPs]
sigForProp e_prop_sigs trans e_prop@(L _ FunBind {..}) =
  case filter isForProp e_prop_sigs of
    [L l (TypeSig e n (HsWC x (L sl (HsSig xs hib t))))] ->
      [L l (TypeSig e n $ HsWC x $ L sl $ HsSig xs hib $ replLast t)]
    _ -> []
  where
    isForProp (L _ (TypeSig _ [L _ r] _)) | r == unLoc fun_id = True
    isForProp _ = False
    -- We add a within to the alternatives, so the type changes from Bool
    -- We make sure that we don't replace wildcard types.
    replLast :: LHsType GhcPs -> LHsType GhcPs
    replLast (L l (HsQualTy hqtx hqctxt r )) =
      L l $ HsQualTy hqtx hqctxt $ replLast r
    replLast (L l (HsFunTy noAnn arr k r)) =
      L l $ HsFunTy noAnn arr k $ replLast r
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
  EProb {..} = (num_args, noLocA $ HsLet noAnn noHsTok ctxt noHsTok check_prog)
    where
      HsValBinds be vb = e_ctxt
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
      ctxt = HsValBinds be nvb
      propWithin = L loc fb {fun_matches = fm {mg_alts = L lm malts'}}

      malts' = map addWithin malts
      addWithin (L l m@Match {m_grhss = grhs@GRHSs {grhssGRHSs = bs}}) =
        L l m {m_grhss = grhs {grhssGRHSs = bs'}}
        where
          bs' = map aW bs
          aW (L l' (GRHS x h b)) = L l' (GRHS x h b')
            where
              b' =
                noLocA $
                  HsApp
                    noAnn
                    (noLocA $ HsApp noAnn (tf "qcWithin") (il timeout))
                    (noLocA $ HsPar noAnn noHsTok b noHsTok)
      sq_ty :: LHsSigWcType GhcPs
      sq_ty =
        HsWC NoExtField $
          noLocA $
            HsSig NoExtField (HsOuterImplicit NoExtField) $
            noLocA $
              HsAppTy
                NoExtField
                (tt "IO")
                ( noLocA $
                    HsParTy noAnn $
                      noLocA
                        ( HsAppTy
                            NoExtField
                            (tt "Maybe")
                            (noLocA $ HsListTy noAnn $ tt "String")
                        )
                )
      check_prog :: LHsExpr GhcPs
      check_prog =
        noLocA $
          HsPar noAnn noHsTok
            (progAtTy
              ( noLocA $
                  HsPar noAnn noHsTok 
                    (fromJust $ testCheckExpr e_prog cc (tf "failureToMaybe", tf "id") prop) 
                    noHsTok
              )
              sq_ty) noHsTok
buildCounterExampleCheck _ _ _ _ = error "invalid counter-example format!"

