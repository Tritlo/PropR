{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : PropR.Eval
-- Description : Contains most parts that directly rely on GHC Compilation
-- License     : MIT
-- Stability   : experimental
--
-- This module holds most of the methods that interact with the GHC.
-- This is a low-level module. This module is impure.
-- This consists of the following blocks:
--
-- 1. Parsing a given problem from String into actual expressions of the Code
-- 2. Compiling given expression and their types, e.g. to check for hole fits later
-- 3. Finding Candidates for Genetic Programming
-- 4. Configuration for this and other parts of the project
module PropR.Eval where

import Bag (Bag, bagToList, concatBag, concatMapBag, emptyBag, listToBag, mapBag, mapMaybeBag, unitBag)
import Constraint
import Control.Applicative (Const)
import Control.Arrow (second, (***))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens (Getting, to, universeOf, universeOn, universeOnOf)
import Control.Lens.Combinators (Fold)
import Control.Monad (forM, forM_, unless, void, when, zipWithM_, (>=>))
import qualified CoreUtils
import qualified Data.Bifunctor
import Data.Bits (complement)
import Data.Char (isAlphaNum)
import Data.Data.Lens (template, tinplate, uniplate)
import Data.Dynamic (Dynamic, fromDynamic)
import Data.Function (on)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.List (groupBy, intercalate, nub, partition, stripPrefix)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, isNothing, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Data.Tree (Tree (Node, rootLabel), flatten)
import Desugar (deSugarExpr)
import DsExpr (dsLExpr, dsLExprNoLP)
import DsMonad (initDsTc, initDsWithModGuts)
import DynFlags
import PropR.Check
import PropR.Configuration
import PropR.Plugin
import PropR.Traversals (flattenExpr)
import PropR.Types
import PropR.Util
import ErrUtils (ErrMsg (errMsgSeverity), errMsgSpan, pprErrMsgBagWithLoc)
import FV (fvVarSet)
import GHC
import GHC.LanguageExtensions (Extension (ExtendedDefaultRules, PartialTypeSignatures))
import GHC.Paths (libdir)
import GHC.Prim (unsafeCoerce#)
import GhcPlugins hiding (exprType)
import Numeric (showHex)
import PrelNames (pRELUDE_NAME, toDynName)
import RnExpr (rnLExpr)
import StringBuffer (stringToStringBuffer)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, makeAbsolute, removeDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (Handle, hClose, hGetLine, openTempFile)
import System.Posix.Process
import System.Posix.Signals
import System.Process
import qualified System.Timeout (timeout)
import TcExpr (tcInferSigma)
import TcHoleErrors (HoleFit (..), TypedHole (..))
import TcSimplify (captureTopConstraints)
import Trace.Hpc.Mix
import Trace.Hpc.Tix (Tix (Tix), TixModule (..), readTix, tixModuleName)
import Trace.Hpc.Util (HpcPos, fromHpcPos)
import TyCoRep

-- Configuration and GHC setup

holeFlags :: [GeneralFlag]
holeFlags =
  [ Opt_ShowHoleConstraints,
    Opt_ShowProvOfHoleFits,
    Opt_ShowTypeAppVarsOfHoleFits,
    Opt_ShowTypeAppOfHoleFits,
    Opt_ShowTypeOfHoleFits
  ]

setFlags :: [GeneralFlag]
setFlags = [Opt_Hpc]

exts :: [Extension]
exts = [PartialTypeSignatures, ExtendedDefaultRules]

config :: Int -> DynFlags -> DynFlags
config lvl sflags =
  -- turn-off all warnings
  flip (foldl wopt_unset) [toEnum 0 ..] $
    flip (foldl xopt_set) exts $
      flip (foldl gopt_set) setFlags $
        (foldl gopt_unset sflags (Opt_OmitYields : holeFlags))
          { maxValidHoleFits = Nothing,
            maxRefHoleFits = Nothing,
            refLevelHoleFits = Just lvl
          }

----

-- | This method takes a package given as a string and puts it into the GHC PackageFlag-Type
toPkg :: String -> PackageFlag
toPkg str = ExposePackage ("-package " ++ str) (PackageArg str) (ModRenaming True [])

-- | Initializes the context and the hole fit plugin with no
-- expression fit candidates
initGhcCtxt :: CompileConfig -> Ghc (IORef HoleFitState)
initGhcCtxt cc = initGhcCtxt' False cc []

-- | Intializes the hole fit plugin we use to extract fits and inject
-- expression fits, as well as adding any additional imports.
initGhcCtxt' ::
  Bool ->
  -- | Whether to use Caching
  CompileConfig ->
  -- | The experiment configuration
  [ExprFitCand] ->
  Ghc (IORef HoleFitState)
initGhcCtxt' use_cache cc@CompConf {..} local_exprs = do
  liftIO $ logStr TRACE "Initializing GHC..."
  -- First we have to add "base" to scope
  flags <- config holeLvl <$> getSessionDynFlags
  --`dopt_set` Opt_D_dump_json
  plugRef <- liftIO $ newIORef initialHoleFitState
  let flags' =
        flags
          { packageFlags =
              packageFlags flags
                ++ map toPkg (checkPackages ++ packages),
            staticPlugins = sPlug : staticPlugins flags
          }
      sPlug =
        StaticPlugin $
          PluginWithArgs
            { paArguments = [],
              paPlugin = synthPlug cc use_cache local_exprs plugRef
            }
  -- "If you are not doing linking or doing static linking, you can ignore the list of packages returned."
  liftIO $ logStr TRACE "Setting DynFlags..."
  -- We might get "congestion" if multiple GHC threads are all making .mix files

  toLink <- setSessionDynFlags flags' {importPaths = importPaths flags' ++ modBase}
  -- (hsc_dynLinker <$> getSession) >>= liftIO . (flip extendLoadedPkgs toLink)
  -- Then we import the prelude and add it to the context
  liftIO $ logStr TRACE "Parsing imports..."
  imports <- addPreludeIfNotPresent <$> mapM (fmap IIDecl . parseImportDecl) importStmts
  let toTarget mod_path = Target (TargetFile mod_path Nothing) True Nothing
  liftIO $ logStr TRACE "Adding additional targets..."
  mapM_ (addTarget . toTarget) additionalTargets
  liftIO $ logStr TRACE "Loading targets.."
  _ <- load LoadAllTargets
  liftIO $ logStr TRACE "Adding imports to context..."
  getContext >>= setContext . (imports ++)
  liftIO $ logStr TRACE "Initialization complete."
  return plugRef

addPreludeIfNotPresent :: [InteractiveImport] -> [InteractiveImport]
addPreludeIfNotPresent decls =
  if any isPrelude decls
    then decls
    else prelImport : decls
  where
    isPrelude (IIModule mname) = mname == pRELUDE_NAME
    isPrelude (IIDecl ImportDecl {..}) = unLoc ideclName == pRELUDE_NAME
    isPrelude _ = False
    prelImport = IIDecl $ simpleImportDecl pRELUDE_NAME

justParseExpr :: CompileConfig -> RExpr -> Ghc (LHsExpr GhcPs)
justParseExpr cc str = do
  _ <- initGhcCtxt cc
  parseExprNoInit str

parseExprNoInit :: HasCallStack => RExpr -> Ghc (LHsExpr GhcPs)
parseExprNoInit str =
  handleSourceError
    (\err -> printException err >> error ("parse failed in: `" ++ str ++ "`"))
    (parseExpr str)

type ValsAndRefs = ([HoleFit], [HoleFit])

-- |
--  The compiler result, which can either be a set of values and refs (everything
--  worked) or still be dynamic, which means that some kind of error occurred.
--  That could be that the holes are not resolvable, the program does not clearly
--  terminate etc.
type CompileRes = Either [ValsAndRefs] Dynamic

-- | By integrating with a hole fit plugin, we can extract the fits (with all
-- the types and everything directly, instead of having to parse the error
-- message)
getHoleFitsFromError ::
  IORef HoleFitState ->
  [SrcSpan] ->
  SourceError ->
  Ghc (Either (Either ([ValsAndRefs], [ErrMsg]) [ValsAndRefs]) b)
getHoleFitsFromError plugRef holeSpanList err =
  do
    liftIO $ logOut DEBUG $ pprErrMsgBagWithLoc $ srcErrorMessages err
    res <- liftIO $ snd <$> readIORef plugRef
    when (null res) (printException err)
    let gs = groupBy (sameHole `on` fst) res
        allFitsOfHole ((th, f) : rest) = (th, concat $ f : map snd rest)
        allFitsOfHole [] = error "no-holes!"
        valsAndRefs = map ((partition part . snd) . allFitsOfHole) gs
    let msgs = srcErrorMessages err
        errSpans =
          Set.fromList $
            map errMsgSpan $ filter ((==) "SevError" . show . errMsgSeverity) $ bagToList msgs
        -- There might be other erros present, which means that
        -- the hole was not sound (probably due to defaulting).
        -- We want to filter these out.
        otherErrorSpans = errSpans Set.\\ holeSpans
        otherErrors = filter ((`Set.member` otherErrorSpans) . errMsgSpan) $ bagToList msgs

    if not (null otherErrorSpans)
      then do
        liftIO $ logStr TRACE "Additional errors detected, discarding"
        liftIO $ mapM_ (logOut DEBUG) valsAndRefs
        return $ Left $ Left $ (valsAndRefs, otherErrors)
      else return $ Left $ Right valsAndRefs
  where
    noRes = ([], [])
    holeSpans = Set.fromList holeSpanList
    part (RawHoleFit _) = True
    part HoleFit {..} = hfRefLvl <= 0
    sameHole :: TypedHole -> TypedHole -> Bool
    sameHole
      TyH {tyHCt = Just CHoleCan {cc_hole = h1}}
      TyH {tyHCt = Just CHoleCan {cc_hole = h2}} =
        holeOcc h1 == holeOcc h2
    sameHole _ _ = False

addTargetGetModName :: Target -> Ghc ModuleName
addTargetGetModName target = do
  mnames_before <- Set.fromList . map ms_mod_name . mgModSummaries <$> depanal [] False
  addTarget target
  mnames_after <- Set.fromList . map ms_mod_name . mgModSummaries <$> depanal [] False
  return $ Set.findMin $ mnames_after `Set.difference` mnames_before

-- |
--  This method tries attempts to parse a given Module into a repair problem.
moduleToProb ::
  CompileConfig ->
  -- | A given Compilerconfig to use for the Module
  FilePath ->
  -- | The Path under which the module is located
  Maybe String ->
  -- | "mb_target" whether to target a specific type (?)
  IO (CompileConfig, TypecheckedModule, Maybe EProblem)
moduleToProb baseCC@CompConf {tempDirBase = baseTempDir, ..} mod_path mb_target = do
  modHash <- flip showHex "" . abs . hashString <$> readFile mod_path

  let tdBase = baseTempDir </> modHash </> dropExtensions mod_path
      cc@CompConf {..} = baseCC {tempDirBase = tdBase}

  let target_id = TargetFile mod_path Nothing
      target = Target target_id True Nothing

  -- Feed the given Module into GHC
  runGhc' cc {importStmts = importStmts ++ checkImports, randomizeHiDir = False} $ do
    dflags <- getSessionDynFlags
    liftIO $ logStr TRACE "Loading module targets..."

    mname <- addTargetGetModName target
    let no_ext = dropExtension mod_path
        thisModBase = case stripPrefix (reverse $ moduleNameSlashes mname) no_ext of
          Just dir -> reverse dir
          _ -> dropFileName no_ext
        orig_mname = mname
        local_paths = thisModBase : modBase
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags {importPaths = importPaths dflags ++ local_paths}
    mnames_after_local <- depanal [] False
    _ <- load LoadAllTargets
    -- Retrieve the parsed module
    liftIO $ logStr TRACE "Parsing module..."
    modul@ParsedModule {..} <- getModSummary mname >>= parseModule
    liftIO $ logStr TRACE "Type checking module..."
    tc_modul@TypecheckedModule {..} <- typecheckModule modul

    -- Due to an issue with the GHC linker, we have to give modules a name
    -- so we can import them. So we create a "FakeMain" module to represent
    -- the unnamed or Main module we're checking. Note: this creates a
    -- file that needs to be deleted later. However: only one such is created
    -- per run.
    (fake_module, fake_import, mname) <-
      if moduleNameString mname == "Main"
        then do
          let fakeMname = intercalate "_" ["FakeMain", modHash, takeBaseName mod_path]
              fakeMainBase = tempDirBase </> "common" </> "build" </> fakeMname
              fakeMainLoc = fakeMainBase <.> "hs"
              mod :: HsModule GhcPs
              mod = unLoc pm_parsed_source
              (toRemove, exportStr) =
                case hsmodName mod of
                  -- A truly unnamed module
                  Nothing -> (Nothing, "")
                  Just (L (RealSrcSpan rsp) mname) ->
                    ( Just rsp,
                      case hsmodExports mod of
                        Nothing -> ""
                        Just (L _ exports) ->
                          let exps =
                                intercalate "," $
                                  map showUnsafe exports
                           in ("(" ++ exps ++ ")")
                    )
                  _ -> (Nothing, "")

          contents <- liftIO $ lines <$> readFile mod_path
          -- We ignore the exportStr, since we're fixing things local to the
          -- module. TODO: ignore exports on other local modules as well
          let mhead = unwords ["module", fakeMname {-- exportStr, --}, "where", "\n"]
              filtered =
                case toRemove of
                  Nothing -> mhead : contents
                  Just rsp ->
                    take (srcSpanStartLine rsp - 1) contents
                      ++ [mhead]
                      ++ drop (srcSpanEndLine rsp) contents
          liftIO $ writeFile fakeMainLoc $ unlines filtered
          let fake_target_id = TargetFile fakeMainLoc Nothing
              fake_target = Target fake_target_id True Nothing

          removeTarget target_id
          fake_mname <- addTargetGetModName fake_target

          setSessionDynFlags
            dflags
              { mainModIs = mkModule mainUnitId fake_mname
              }
          _ <- load LoadAllTargets
          return (fakeMainLoc, unwords ["import", fakeMname], fake_mname)
        else return ([], [], mname)

    let (L _ HsModule {..}) = pm_parsed_source
        cc' =
          cc
            { -- We import the module itself to get all instances and data
              -- declarations in scope.
              importStmts =
                ( if moduleNameString orig_mname /= "Main"
                    then self_import
                    else fake_import
                ) :
                rest_imports,
              modBase = dropFileName mod_path : modBase,
              additionalTargets =
                ( if moduleNameString orig_mname /= "Main"
                    then mod_path
                    else fake_module
                ) :
                additionalTargets
            }
          where
            imps' = map showUnsafe hsmodImports
            self_import = "import " ++ moduleNameString mname
            self_target = mod_path
            rest_imports = importStmts ++ imps'
        -- Retrieves the Values declared in the given Haskell-Module
        valueDeclarations :: [LHsBind GhcPs]
        valueDeclarations = mapMaybe fromValD hsmodDecls
          where
            fromValD (L l (ValD _ b)) = Just (L l b)
            fromValD _ = Nothing
        -- Retrieves the Sigmas declared in the given Haskell-Module
        sigmaDeclarations :: [LSig GhcPs]
        sigmaDeclarations = mapMaybe fromSigD hsmodDecls
          where
            fromSigD (L l (SigD _ s)) = Just (L l s)
            fromSigD _ = Nothing

        toCtxt :: [LHsBind GhcPs] -> [LSig GhcPs] -> LHsLocalBinds GhcPs
        toCtxt vals sigs =
          noLoc $ HsValBinds NoExtField (ValBinds NoExtField (listToBag vals) (sigmaDeclarations ++ sigs))

        tests :: Set OccName
        tests = tastyTests `Set.union` qcProps
        tastyTests = Set.fromList testTreeList
        (testTreeList, qcProps) = Set.fromList <$> tjoin tm_typechecked_source
          where
            tjoin :: LHsBinds GhcTc -> ([OccName], [OccName])
            tjoin binds =
              let (tt, ps) = unzip $ bagToList $ mapBag fromTPropD binds
               in (mconcat tt, mconcat ps)
            fromTPropD :: LHsBind GhcTc -> ([OccName], [OccName])
            fromTPropD b@(L l FunBind {..})
              | t <- idType $ unLoc fun_id,
                isTestTree t || isProp t (unLoc fun_id) =
                let res = [getOccName $ unLoc fun_id]
                 in if isTestTree t
                      then (res, mempty)
                      else (mempty, res)
            fromTPropD b@(L l VarBind {..})
              | t <- idType var_id,
                isTestTree t || isProp t var_id =
                let res = [getOccName var_id]
                 in if isTestTree t
                      then (res, mempty)
                      else (mempty, res)
            fromTPropD b@(L l AbsBinds {..}) = tjoin abs_binds
            fromTPropD _ = (mempty, mempty)
            isTestTree (TyConApp tt _) =
              ((==) "TestTree" . occNameString . getOccName) tt
            isTestTree _ = False
            -- TODO: Check the type of the fun_id as well, or exclusively
            isProp :: Type -> Id -> Bool
            isProp _ = (==) "prop" . take 4 . occNameString . occName

    -- We want to unfold the tests:
    unfoldedTasty <-
      if unfoldTastyTests
        then do
          liftIO $ logStr TRACE "Unfolding tests..."
          let thisMod = IIDecl $ simpleImportDecl mname
          getContext >>= setContext . (thisMod :)
          imports <- addPreludeIfNotPresent <$> mapM (fmap IIDecl . parseImportDecl) (importStmts ++ checkImports)
          getContext >>= setContext . ((thisMod : imports) ++)
          let countExpr =
                "map (length . unfoldTastyTests) ["
                  ++ intercalate ", " (map showUnsafe testTreeList)
                  ++ "]"
          countTrees <- compileExpr countExpr
          let treeLengths :: [Int]
              treeLengths = unsafeCoerce# countTrees
          return $ Map.fromList $ zip testTreeList treeLengths
        else return Map.empty

    let props :: [LHsBind GhcPs]
        props = mapMaybe fromPropD hsmodDecls
          where
            fromPropD (L l (ValD _ b@FunBind {..}))
              | rdrNameOcc (unLoc fun_id) `elem` tests = Just (L l b)
            fromPropD _ = Nothing
        local_prop_var_names :: [Name]
        local_prop_var_names = filter fromLocalPackage prop_var_names
          where
            fromLocalPackage name
              | Just mod <- nameModule_maybe name =
                mgElemModule mnames_after_local mod
            fromLocalPackage _ = False
            prop_var_names :: [Name]
            prop_var_names = filter ((`Set.member` prop_var_occs) . occName) top_lvl_names
            prop_var_occs = Set.map occName prop_vars
            Just top_lvl_names = modInfoTopLevelScope (moduleInfo tc_modul)
            prop_vars :: Set RdrName
            prop_vars =
              Set.fromList $
                mapMaybe mbVar (universeOnOf tinplate uniplate (toCtxt props []) :: [LHsExpr GhcPs])
            mbVar (L _ (HsVar _ v)) = Just $ unLoc v
            mbVar _ = Nothing

        fix_targets :: [RdrName]
        fix_targets =
          Set.toList $
            Set.filter (not . (`Set.member` excludeSet) . rdrNameToStr) $
              Set.filter ((`Set.member` name_occs) . occName) fun_ids
          where
            name_occs = Set.map occName $ Set.fromList local_prop_var_names
            funId (L _ (ValD _ FunBind {..})) = Just $ unLoc fun_id
            funId _ = Nothing
            fun_ids = Set.fromList $ mapMaybe funId hsmodDecls
            excludeSet = Set.fromList excludeTargets
        getTarget :: [RdrName] -> Maybe EProblem
        getTarget t_names =
          case targets of
            _
              | len <- length targets,
                len > 0,
                len == length t_names,
                exprs <- wp_expr targets_n_sigs,
                tys <- map prog_ty sigs ->
                Just $
                  EProb
                    { e_ctxt = toCtxt valueDeclarations [],
                      e_prog = zip3 targets tys exprs,
                      e_module = Just mname,
                      e_props = wrapped_props,
                      e_prop_sigs = prop_sigs
                    }
            _ | len <- length targets -> Nothing
          where
            t_names_set = Set.fromList t_names
            isTDef (L _ (SigD _ (TypeSig _ ids _))) =
              not (t_names_set `Set.disjoint` Set.fromList (map unLoc ids))
            isTDef (L _ (ValD _ FunBind {..})) =
              unLoc fun_id `Set.member` t_names_set
            isTDef _ = False

            -- We get the type of the program
            getTType t_name (L _ (SigD _ ts@(TypeSig _ ids _)))
              | t_name `elem` map unLoc ids = Just ts
            getTType _ _ = Nothing

            -- takes prop :: t ==> prop' :: target_type -> t since our
            -- previous assumptions relied on the properties to take in the
            -- function being fixed  as the first argument.

            -- wrap prop helpers:
            mkFid nocc (L l' (Unqual occ)) = L l' (Unqual (nocc occ))
            mkFid nocc (L l' (Qual m occ)) = L l' (Qual m (nocc occ))
            mkFid _ b = b

            nmatches vars nfid mg@MG {mg_alts = (L l' alts)} = mg {mg_alts = L l' nalts}
              where
                nalt (L l'' m@Match {..}) = L l'' m {m_pats = nvpats ++ m_pats, m_ctxt = n_ctxt}
                  where
                    n_ctxt =
                      case m_ctxt of
                        fh@FunRhs {mc_fun = L l''' _} ->
                          fh {mc_fun = L l''' $ unLoc nfid}
                        o -> o
                nalt alt = alt
                nalts = map nalt alts
                nvpat t_name =
                  noLoc $
                    ParPat NoExtField $
                      noLoc $ VarPat NoExtField $ noLoc t_name
                nvpats = map nvpat $ filter (`Set.member` vars) targets
            nmatches _ _ mg = mg

            wrapProp :: LHsBind GhcPs -> [(LHsBind GhcPs, [Sig GhcPs])]
            wrapProp prop@(L l fb@FunBind {..})
              | Just num_cases <- unfoldedTasty Map.!? rdr_occ =
                concatMap toBind [0 .. (num_cases -1)]
              where
                prop_vars = propVars prop
                rdr_occ = rdrNameOcc (unLoc fun_id)
                nocc :: Int -> OccName -> OccName
                nocc i o = mkOccName (occNameSpace o) $ occNameString o ++ "__test_" ++ show i
                toBind i =
                  [(L l fb {fun_id = nfid, fun_matches = run_i_only change_target}, sig)]
                  where
                    sig = mkPropSig prop_vars nfid fun_id
                    nfid = mkFid (nocc i) fun_id
                    change_target = nmatches prop_vars nfid fun_matches
                    run_i_only :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
                    run_i_only mg@MG {mg_alts = (L l' alts)} = mg {mg_alts = L l' $ map nalt alts}
                      where
                        nalt (L l'' m@Match {..}) = L l'' m {m_grhss = n_grhss m_grhss}
                        nalt m = m
                        n_grhss grhss@GRHSs {..} = grhss {grhssGRHSs = map nGRHSS grhssGRHSs}
                        n_grhss g = g
                        nGRHSS (L l3 (GRHS x guards bod)) = L l3 (GRHS x guards nbod)
                          where
                            nbod = noLoc $ HsApp ext nthapp (noLoc $ HsPar ext bod)
                            ext = noExtField
                            nthapp = noLoc $ HsPar ext (noLoc $ HsApp ext (tf "testTreeNthTest") (il $ fromIntegral i))
                        nGRHSS xg = xg
                    run_i_only mg = mg
            wrapProp prop@(L l fb@FunBind {..}) =
              [(L l fb {fun_id = nfid, fun_matches = nmatches prop_vars nfid fun_matches}, sig)]
              where
                sig = mkPropSig prop_vars nfid fun_id
                prop_vars = propVars prop
                nfid = mkFid nocc fun_id
                nocc o = mkOccName (occNameSpace o) $ insertAt 4 '\'' $ occNameString o
            wrapProp e = [(e, [])]

            -- We make a binding that uses PartialTypeSignatures
            mkPropSig :: Set RdrName -> Located RdrName -> Located (IdP GhcPs) -> [Sig GhcPs]
            mkPropSig vars nfid ofid = prop_sig
              where
                prop_sig = case snd <$> prog_sig (unLoc ofid) of
                  -- We don't want to wrap wildcardTys any further
                  Just (TypeSig e _ wt) ->
                    [ TypeSig e [nfid] $ case wt of
                        HsWC _ (HsIB _ (L _ (HsWildCardTy _))) -> wt
                        _ -> toWrapSig wt
                    ]
                  _ -> []
                toWrapSig :: LHsSigWcType GhcPs -> LHsSigWcType GhcPs
                toWrapSig (HsWC e (HsIB ix t)) = HsWC e (HsIB ix $ tyApps wtys)
                  where
                    wtys :: [HsType GhcPs]
                    wtys = replicate (length $ filter (`Set.member` vars) targets) (HsWildCardTy NoExtField)
                    tyApps [] = t
                    tyApps (ty : tys) = noLoc $ HsFunTy NoExtField (noLoc ty) (tyApps tys)
                toWrapSig e = e
            wrapped_props :: [EProp]
            prop_sigs :: [LSig GhcPs]
            (wrapped_props, prop_sigs) =
              second (concatMap (noLoc <$>)) $ unzip $ concatMap wrapProp props

            prog_binds :: LHsBindsLR GhcPs GhcPs
            prog_binds = listToBag $ mapMaybe f $ filter isTDef hsmodDecls
              where
                f (L _ (ValD _ b)) = Just $ noLoc b
                f _ = Nothing

            prog_sig :: RdrName -> Maybe (RdrName, Sig GhcPs)
            prog_sig t_name = case mapMaybe (getTType t_name) hsmodDecls of
              (pt : _) ->
                let sig = case pt of
                      TypeSig e sgs wt ->
                        TypeSig e (filter ((t_name ==) . unLoc) sgs) wt
                      _ -> pt
                 in Just (t_name, sig)
              _ ->
                Just
                  ( t_name,
                    -- If we don't have a type, we just make a
                    -- partial one.
                    TypeSig NoExtField [noLoc t_name] $
                      HsWC NoExtField $
                        HsIB NoExtField $
                          noLoc $ HsWildCardTy NoExtField
                  )

            targets_n_sigs = mapMaybe prog_sig t_names
            (targets, sigs) = unzip targets_n_sigs

            prog_ty :: Sig GhcPs -> EType
            prog_ty prog_sig' = sig
              where
                (TypeSig _ _ sig) = prog_sig'
            wp_expr :: [(RdrName, Sig GhcPs)] -> [LHsExpr GhcPs]
            wp_expr tns = map wp' t_names
              where
                (t_names, sigs) = unzip tns
                sigs' = map noLoc sigs
                -- TODO: We should maybe take the transitive closure of
                -- the binds here, *BUT* that would mean that the programs
                -- (and therefore the locations) will be different. Hmm.
                wp' :: RdrName -> LHsExpr GhcPs
                wp' t_name = noLoc $ HsLet noExtField (noLoc lbs) (noLoc le)
                  where
                    le = HsVar noExtField $ noLoc t_name
                    lbs =
                      HsValBinds noExtField $
                        ValBinds noExtField prog_binds sigs'

        int_prob = case mb_target of
          Just t ->
            case getTarget [mkVarUnqual $ fsLit t] of
              Just r -> Just r
              _ -> error $ "Could not find type of the target `" ++ t ++ "`!"
          Nothing -> getTarget fix_targets
    -- We don't do ExProbs yet
    --     let prob = case int_prob of
    --           Nothing -> Just $ ExProb local_prop_var_names
    --           _ -> int_prob
    return (cc', tc_modul, int_prob)

-- | Runs a GHC action and cleans up any hpc-directories that might have been
-- created as a side-effect.
-- TODO: Does this slow things down massively? We lose out on the pre-generated
-- mix and hi files for sure, but we don't get those weird
-- "Exception: tests/cases/ThreeFixes.hi: openBinaryFile: resource busy (file is locked)"
-- or
-- "Exception: .hpc/FourFixes.mix: openFile: resource busy (file is locked)"
-- exceptions.
runGhcWithCleanup :: CompileConfig -> Ghc a -> IO a
runGhcWithCleanup CompConf {..} act = do
  td_seed <- flip showHex "" . abs <$> newSeed
  let tdBase' = tempDirBase </> "run" </> td_seed
  tdBase <- makeAbsolute tdBase'
  let common = tempDirBase </> "common"
      extra_dirs' = [common </> "build", common]
  extra_dirs <- mapM makeAbsolute extra_dirs'
  createDirectoryIfMissing True tdBase

  res <- runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    let dflags' =
          dflags
            { hpcDir =
                if randomizeHpcDir
                  then tdBase </> "hpc"
                  else common </> "hpc",
              importPaths = importPaths dflags ++ extra_dirs,
              libraryPaths = libraryPaths dflags ++ extra_dirs,
              -- Objects are loaded and unloaded  and we can't load them
              -- twice (since that confuses the C linker). Using
              -- Linker.unload makes it segfault, so we'll have to make
              -- do with placing the objects at the same place.
              -- So we can't just set
              -- ```
              -- objectDir = Just (tdBase </> "build"),
              --- ```
              -- we have to put it into a "common" directory (but at least it
              -- can be in the temp directory).
              objectDir = Just (common </> "build"),
              hiDir =
                Just
                  ( if randomizeHiDir
                      then tdBase </> "build"
                      else common </> "build"
                  ),
              log_action = logOutLogAction
            }
    setSessionDynFlags dflags'
    defaultErrorHandler ((logStr GHCERR "ghc error: " >>) . logStr GHCERR) (FlushOut (return ())) act
  check <- doesDirectoryExist tdBase
  when check $ removeDirectoryRecursive tdBase
  return res

logOutLogAction :: LogAction
logOutLogAction dflags _ severity loc _ msgdoc =
  do
    let ll = case severity of
          SevOutput -> DUMP
          SevDump -> DUMP
          SevInteractive -> INFO
          SevInfo -> INFO
          SevWarning -> INFO
          SevFatal -> GHCERR
          SevError -> GHCERR
    logStr ll ("GHC ERROR at " ++ showUnsafe loc)
    logOut ll msgdoc

runGhc' :: CompileConfig -> Ghc a -> IO a
runGhc' cc = runGhcWithCleanup cc . (initGhcCtxt cc >>)

traceTarget ::
  CompileConfig ->
  EProblem ->
  EProgFix ->
  EProp ->
  [RExpr] ->
  IO (Maybe TraceRes)
traceTarget cc tp e fp ce = head <$> traceTargets cc tp e [(fp, ce)]

toNonZeroInvokes :: Trace -> Map.Map (EExpr, SrcSpan) Integer
toNonZeroInvokes (ex, res) = Map.fromList $ mapMaybe only_max $ flatten res
  where
    isOkBox (ExpBox _, _) = True
    isOkBox _ = False
    only_max :: (SrcSpan, [(BoxLabel, Integer)]) -> Maybe ((EExpr, SrcSpan), Integer)
    only_max (src, xs)
      | any isOkBox xs,
        ms <- maximum $ map snd xs,
        ms > 0 =
        Just ((ex, src), ms)
    -- TODO: What does it mean in HPC if there are multiple labels here?
    only_max (src, xs) = Nothing

-- Run HPC to get the trace information.
traceTargets ::
  CompileConfig ->
  EProblem ->
  EProgFix ->
  [(EProp, [RExpr])] ->
  IO [Maybe TraceRes]
traceTargets _ _ _ [] = return []
-- Note: we call runGhc' here directly, since we want every trace to be a
-- new ghc thread (since we set the dynflags per module)
traceTargets cc@CompConf {..} tp@EProb {..} exprs ps_w_ce =
  runGhc' cc $ do
    liftIO $ logStr TRACE "Tracing targets..."
    seed <- liftIO newSeed
    let traceHash = flip showHex "" $ abs $ hashString $ showUnsafe (exprs, ps_w_ce, seed)
        tempDir = tempDirBase </> "trace" </> traceHash
        the_f = tempDir </> ("FakeTraceTarget" ++ traceHash) <.> "hs"
    liftIO $ createDirectoryIfMissing True tempDir
    let correl =
          zipWith
            ( \(nm, _, _) e ->
                let ftn = "fake_target_" ++ rdrNamePrint nm
                 in (ftn, nm, baseFun (mkVarUnqual $ fsLit ftn) e)
            )
            e_prog
            exprs
        fake_target_names = Set.fromList $ map (\(n, _, _) -> n) correl
        mname = filter isAlphaNum $ dropExtension $ takeFileName the_f
        modTxt = exprToTraceModule cc tp seed mname correl ps_w_ce
        exeName = dropExtension the_f
        mixFilePath = tempDir

    liftIO $ writeFile the_f modTxt
    _ <- liftIO $ mapM (logStr DEBUG) $ lines modTxt
    -- We set the module as the main module, which makes GHC generate
    -- the executable.
    dynFlags <- getSessionDynFlags
    _ <-
      setSessionDynFlags $
        dynFlags
          { hpcDir = tempDir,
            importPaths = importPaths dynFlags ++ [tempDir],
            libraryPaths = libraryPaths dynFlags ++ [tempDir]
          }
    now <- liftIO getCurrentTime
    let tid = TargetFile the_f Nothing
        target = Target tid True Nothing

    -- Adding and loading the target causes the compilation to kick
    -- off and compiles the file.
    target_name <- addTargetGetModName target
    dynFlags <- getSessionDynFlags
    _ <-
      setSessionDynFlags $
        dynFlags
          { mainModIs = mkModule mainUnitId target_name,
            mainFunIs = Just "main__",
            importPaths = importPaths dynFlags ++ modBase
          }

    liftIO $ logStr TRACE "Loading trace target..."
    (_, errs) <- tryGHCCaptureOutput (load (LoadUpTo target_name))
    if not (null errs)
      then do
        liftIO $ logStr TRACE "Got an error while loading trace target!"
        liftIO $ mapM_ (logStr DEBUG) errs
        -- TODO: We could be more clever here and try to figure out which
        -- ps_w_ce is failing... but if it's a weird parse error or some such
        -- (like in #103) there's not much we can do. Beyond just returning
        -- Nothing for all of them.
        return (replicate (length ps_w_ce) Nothing)
      else do
        liftIO $ logStr TRACE "Loaded!"
        liftIO $ logStr TRACE $ "Got " ++ show (length errs) ++ " errors!"

        modul@ParsedModule {pm_parsed_source = (L _ HsModule {..})} <-
          getModSummary target_name >>= parseModule
        -- Retrieves the Values declared in the given Haskell-Module
        let our_targets :: Map String (LHsExpr GhcPs)
            our_targets = Map.fromList $ mapMaybe fromValD hsmodDecls
              where
                fromValD (L l (ValD _ b@FunBind {..}))
                  | rdrNameToStr (unLoc fun_id) `elem` fake_target_names,
                    Just ex <- unFun b =
                    Just (rdrNameToStr $ unLoc fun_id, ex)
                fromValD _ = Nothing

        -- We should for here in case it doesn't terminate, and modify
        -- the run function so that it use the trace reflect functionality
        -- to timeout and dump the tix file if possible.
        let runTrace which = do
              let tixFilePath = exeName ++ "_" ++ show @Integer which ++ ".tix"
              (_, _, _, ph) <-
                createProcess
                  (proc exeName [show which])
                    { env = Just [("HPCTIXFILE", tixFilePath)],
                      -- We ignore the output
                      std_out = CreatePipe
                    }
              ec <- System.Timeout.timeout timeoutVal $ waitForProcess ph

              let -- If it doesn't respond to signals, we can't do anything
                  -- other than terminate
                  loop :: Maybe ExitCode -> Integer -> IO ()
                  loop _ 0 =
                    -- "WHY WON'T YOU DIE?" -- Freddy Kruger
                    getPid ph >>= \case
                      Just pid ->
                        let term3 0 = return ()
                            term3 n = do
                              terminateProcess ph
                              threadDelay timeoutVal
                              c <- isJust <$> getPid ph
                              when c $ term3 (n -1)
                         in term3 3
                      _ -> terminateProcess ph
                  loop Nothing n = do
                    -- If it's taking too long, it's probably stuck in a loop.
                    -- By sending the right signal though, it will dump the tix
                    -- file before dying.
                    mb_pid <- getPid ph
                    case mb_pid of
                      Just pid ->
                        do
                          signalProcess keyboardSignal pid
                          ec2 <- System.Timeout.timeout timeoutVal $ waitForProcess ph
                          loop ec2 (n -1)
                      -- It finished in the brief time between calls, so we're good.
                      _ -> return ()
                  loop (Just _) _ = return ()
              -- We give it 3 tries
              loop ec 3

              tix <- readTix tixFilePath
              -- TODO: When run in parallel, this can fail
              let rm m = (m,) <$> readMix [mixFilePath] (Right m)
                  isTargetMod = (mname ==) . tixModuleName
                  toDom :: (TixModule, Mix) -> [MixEntryDom [(BoxLabel, Integer)]]
                  toDom (TixModule _ _ _ ts, Mix _ _ _ _ es) =
                    createMixEntryDom $ zipWith (\t (pos, bl) -> (pos, (bl, t))) ts es

                  nd n@Node {rootLabel = (root, _)} =
                    fmap (Data.Bifunctor.first (toSpan the_f)) n
                  wTarget n@Node {rootLabel = (_, [(TopLevelBox [ftn], _)])} =
                    (,nd n) <$> our_targets Map.!? ftn
                  wTarget _ = Nothing
                  -- We convert the HpcPos to the equivalent span we would get if we'd
                  -- parsed and compiled the expression directly.
                  toSpan :: FilePath -> HpcPos -> SrcSpan
                  toSpan the_f sp = mkSrcSpan start end
                    where
                      fname = fsLit the_f
                      (sl, sc, el, ec) = fromHpcPos sp
                      start = mkSrcLoc fname sl sc
                      -- HPC and GHC have different philosophies
                      end = mkSrcLoc fname el (ec + 1)
              case tix of
                Just (Tix mods) -> do
                  -- We throw away any extra functions in the file, such as
                  -- the properties and the main function, and only look at
                  -- the ticks for our expression
                  let fake_only = filter isTargetMod mods
                  res <- mapMaybe wTarget . concatMap toDom <$> mapM rm fake_only
                  return $ Just res
                _ -> return Nothing

        res <- liftIO $ mapM (runTrace . fst) $ zip [0 ..] ps_w_ce
        cleanupAfterLoads tempDir mname dynFlags
        return res
  where
    timeoutVal = fromIntegral timeout
traceTargets _ _ [] _ = error "No fix!"
traceTargets _ ExProb {} _ _ = error "Exprobs not supported!"

cleanupAfterLoads :: FilePath -> String -> DynFlags -> Ghc ()
cleanupAfterLoads tempDir mname dynFlags = do
  liftIO $ do
    -- Remove the dir with the .hs and .hi file
    check <- doesDirectoryExist tempDir
    when check $ do
      logStr TRACE $ "Removing " ++ tempDir
      removeDirectoryRecursive tempDir
    case objectDir dynFlags of
      Just dir | fp <- dir </> mname ->
        forM_ ["o", "hi"] $ \ext -> do
          let file = fp <.> ext
          check <- doesFileExist file
          when check $ do
            logStr TRACE $ "Removing " ++ file
            removeFile file
      _ -> return ()

exprToTraceModule ::
  CompileConfig ->
  EProblem ->
  Int ->
  String ->
  [(String, RdrName, LHsBind GhcPs)] ->
  [(EProp, [RExpr])] ->
  RExpr
exprToTraceModule CompConf {..} EProb {..} seed mname fake_targets ps_w_ce =
  unlines $
    ["module " ++ mname ++ " where"]
      ++ importStmts
      ++ checkImports
      -- We include the context if we are doing an inline problem,
      -- otherwise it's already provided by the imports.
      ++ (if isJust e_module then [] else lines $ showUnsafe e_ctxt)
      ++ concatMap (lines . showUnsafe) failing_props
      ++ concatMap (lines . showUnsafe) prop_sigs
      ++ map (showUnsafe . (\(_, _, b) -> b)) fake_targets
      ++ [concat ["checks = [", checks, "]"]]
      ++ [ "",
           "main__ :: IO ()",
           "main__ = do [which] <- getArgs",
           "            act <- checks !! (read which)",
           "            print (act :: Bool) "
         ]
  where
    (failing_props, failing_argss) = unzip ps_w_ce
    prop_sigs = concatMap (sigForProp e_prop_sigs id) failing_props
    toName :: LHsBind GhcPs -> String
    toName (L _ FunBind {fun_id = fid}) = showUnsafe fid
    toName (L _ VarBind {var_id = vid}) = showUnsafe vid
    toName _ = error "Unsupported bind!"
    nas = zip failing_props failing_argss
    toCall prop args
      | pname <- toName prop,
        "prop" /= take 4 pname,
        pvars <- propVars prop,
        fts <- filter (\(_, nm, _) -> nm `Set.member` pvars) fake_targets,
        fts_to_use <- if null fts then fake_targets else fts =
        "checkTastyTree " ++ show timeout ++ " ("
          ++ pname
          ++ " "
          ++ unwords (map (\(n, _, _) -> n) fts_to_use)
          ++ " {- args start here -} "
          ++ unwords args
          ++ ")"
    toCall {- args start here -} prop args
      | pname <- toName prop,
        pvars <- propVars prop,
        fts <- filter (\(_, nm, _) -> nm `Set.member` pvars) fake_targets,
        fts_to_use <- if null fts then fake_targets else fts =
        "fmap qcSuccess ("
          ++ "qcWRes "
          ++ show timeout
          ++ " ("
          ++ showUnsafe (qcArgsExpr seed Nothing)
          ++ ") ("
          ++ pname
          ++ " "
          ++ unwords (map (\(n, _, _) -> n) fts_to_use)
          ++ " {- args start here -} "
          ++ unwords args
          ++ "))"
    checks :: String
    checks = intercalate ", " $ map (uncurry toCall) nas
exprToTraceModule _ _ _ _ _ _ = error "External problems not supported yet!"

-- | Prints the error and stops execution
reportError :: (HasCallStack, GhcMonad m, Outputable p) => p -> SourceError -> m b
reportError p e = do
  liftIO $ do
    logStr ERROR "Compiling check failed with unexpected exception:"
    logStr ERROR (showUnsafe p)
    logStr ERROR (unlines $ bagToList $ mapBag show $ srcErrorMessages e)
  printException e
  error "UNEXPECTED EXCEPTION"

-- | Tries an action, returning Nothing in case of error
nothingOnError :: GhcMonad m => m a -> m (Maybe a)
nothingOnError act = handleSourceError (const $ return Nothing) (Just <$> act)

-- | Tries an action, reports about it in case of error
reportOnError :: (GhcMonad m, Outputable t) => (t -> m a) -> t -> m a
reportOnError act a = handleSourceError (reportError a) (act a)

-- | Adapted from dynCompileExpr in InteractiveEval
dynCompileParsedExpr :: GhcMonad m => LHsExpr GhcPs -> m Dynamic
dynCompileParsedExpr parsed_expr = do
  let loc = getLoc parsed_expr
      to_dyn_expr =
        mkHsApp
          (L loc . HsVar noExtField . L loc $ getRdrName toDynName)
          parsed_expr
  hval <- compileParsedExpr to_dyn_expr
  return (unsafeCoerce# hval :: Dynamic)

exprToCheckModule ::
  CompileConfig ->
  Int ->
  String ->
  EProblem ->
  [EProgFix] ->
  RExpr
exprToCheckModule cc@CompConf {..} seed mname tp fixes =
  unlines $
    ["module " ++ mname ++ " where"]
      ++ importStmts
      ++ checkImports
      ++ lines (showUnsafe ctxt)
      ++ lines (showUnsafe check_bind)
      ++ [ "",
           "runC__ :: Bool -> Int -> IO [Bool]",
           "runC__ pr which = do let f True  = 1",
           "                         f False = 0",
           "                     act <- checks__ !! which",
           "                     if pr then (putStrLn (concat (map (show . f) act))) else return ()",
           "                     return act"
         ]
      ++ [ "",
           -- We can run multiple in parallell, but then we will have issues
           -- if any of them loop infinitely.
           "main__ :: IO ()",
           "main__ = do whiches <- getArgs",
           "            mapM_ (runC__ True . read) whiches"
         ]
  where
    (ctxt, check_bind) = buildFixCheck cc seed tp fixes

-- | Parse, rename and type check an expression
justTcExpr :: EExpr -> Ghc (Maybe ((LHsExpr GhcTc, Type), WantedConstraints))
justTcExpr parsed = do
  hsc_env <- getSession
  (_, res) <-
    liftIO $
      runTcInteractive hsc_env $ captureTopConstraints $ rnLExpr parsed >>= tcInferSigma . fst
  return res

-- | We get the type of the given expression by desugaring it and getting the type
-- of the resulting Core expression
getExprTys :: HscEnv -> [LHsExpr GhcTc] -> Ghc [Maybe Type]
getExprTys hsc_env exprs = do
  liftIO $ logStr TRACE "Desugaring..."
  mb_types <- liftIO $ mapM (fmap snd . deSugarExpr hsc_env) exprs
  return $ map (fmap CoreUtils.exprType) mb_types

-- | Takes an expression and generates HoleFitCandidates from every subexpresson.
-- Uses either the expression itself or the desugared module
getExprFitCands ::
  -- | The expression or module
  Either EExpr TypecheckedModule ->
  Ghc [ExprFitCand]
getExprFitCands expr_or_mod = do
  liftIO $ logStr TRACE "Getting expression fit cands..."
  -- setSessionDynFlags reads the package database.
  liftIO $ logStr TRACE "Reading the package database..."
  _ <- setSessionDynFlags =<< getSessionDynFlags
  -- If it type checks, we can use the expression
  liftIO $ logStr TRACE "Typechecking the expression..."
  case expr_or_mod of
    Left expr -> do
      mb_tcd_context <- justTcExpr expr
      let esAndNames =
            case mb_tcd_context of
              Just ((tcd_context, _), wc) ->
                -- We get all the expressions in the program here,
                -- so that we can  pass it along to our custom holeFitPlugin.
                let flat = flattenExpr tcd_context
                    -- We remove the ones already present and drop the first one
                    -- (since it will be the program itself)
                    flat' = filter nonTriv $ tail flat
                 in toEsAnNames wc flat'
              _ -> []
      liftIO $ logStr TRACE "Getting the session..."
      hsc_env <- getSession
      -- After we've found the expressions and any ids contained within them, we
      -- need to find their types
      liftIO $ logStr TRACE "Getting expression types..."
      mb_tys <- getExprTys hsc_env $ map (\(e, _, _) -> e) esAndNames
      return $ zipWith finalize esAndNames mb_tys
    Right typechecked -> do
      let exprs :: [LHsExpr GhcTc]
          exprs = universeOnOf tinplate uniplate $ typecheckedSource typechecked
          -- TODO: is it OK to ignore the wcs here? Should be.
          esAndNames = toEsAnNames emptyWC $ filter nonTriv exprs
      desugared <- desugarModule typechecked

      liftIO $ logStr TRACE "Getting the session..."
      hsc_env <- getSession
      -- After we've found the expressions and any ids contained within them, we
      -- need to find their types
      liftIO $ logStr TRACE "Getting expression types..."
      mb_tys <-
        liftIO $
          mapM
            ( fmap (fmap CoreUtils.exprType . snd)
                . initDsWithModGuts hsc_env (coreModule desugared)
                . dsLExpr
                . (\(e, _, _) -> e)
            )
            esAndNames
      return $ zipWith finalize esAndNames mb_tys
  where
    toEsAnNames wc = map (\e -> (e, bagToList $ wc_simple wc, concatMap e_ids $ flattenExpr e))
    e_ids (L _ (HsVar _ v)) = [unLoc v]
    e_ids (L _ (HsWrap _ w v)) = concatMap e_ids $ flattenExpr $ noLoc v
    e_ids _ = []
    -- Vars are already in scope
    nonTriv :: LHsExpr GhcTc -> Bool
    nonTriv (L _ HsVar {}) = False
    -- We don't want more holes
    nonTriv (L _ HsUnboundVar {}) = False
    -- We'll get whatever expression is within the parenthesis
    -- or wrap anyway
    nonTriv (L _ HsPar {}) = False
    nonTriv (L _ HsWrap {}) = False
    nonTriv _ = True
    finalize :: (LHsExpr GhcTc, [Ct], [Id]) -> Maybe Type -> ExprFitCand
    finalize (e, _, rs) ty@Nothing = EFC (parenthesizeHsExpr appPrec e) emptyBag rs ty
    finalize (e, wc, rs) ty@(Just expr_ty) =
      EFC (parenthesizeHsExpr appPrec e) (listToBag (relevantCts expr_ty wc)) rs ty
    -- Taken from TcHoleErrors, which is sadly not exported. Takes a type and
    -- a list of constraints and filters out irrelvant constraints that do not
    -- mention any typve variable in the type.
    relevantCts :: Type -> [Ct] -> [Ct]
    relevantCts expr_ty simples =
      if isEmptyVarSet (fvVarSet expr_fvs')
        then []
        else filter isRelevant simples
      where
        ctFreeVarSet :: Ct -> VarSet
        ctFreeVarSet = fvVarSet . tyCoFVsOfType . ctPred
        expr_fvs' = tyCoFVsOfType expr_ty
        expr_fv_set = fvVarSet expr_fvs'
        anyFVMentioned :: Ct -> Bool
        anyFVMentioned ct =
          not $
            isEmptyVarSet $
              ctFreeVarSet ct `intersectVarSet` expr_fv_set
        -- We filter out those constraints that have no variables (since
        -- they won't be solved by finding a type for the type variable
        -- representing the hole) and also other holes, since we're not
        -- trying to find hole fits for many holes at once.
        isRelevant ct =
          not (isEmptyVarSet (ctFreeVarSet ct))
            && anyFVMentioned ct
            && not (isHoleCt ct)

-- | Provides a GHC without a ic_default set.
-- IC is short for Interactive-Context (ic_default is set to empty list).
withDefaulting :: Maybe [Type] -> Ghc a -> Ghc a
withDefaulting tys act = do
  -- Make sure we don't do too much defaulting by setting `default ()`
  -- Note: I think this only applies to the error we would be generating,
  -- I think if we replace the UnboundVar with a suitable var of the right
  -- it would work... it just makes the in-between output a bit confusing.
  env <- getSession
  let prev = ic_default (hsc_IC env)
  setSession (env {hsc_IC = (hsc_IC env) {ic_default = tys}})
  r <- act
  env' <- getSession
  setSession (env' {hsc_IC = (hsc_IC env') {ic_default = prev}})
  return r

-- Runs the given action and captures any errors
tryGHCCaptureOutput :: Ghc a -> Ghc (a, [String])
tryGHCCaptureOutput act = do
  msg_ref <- liftIO $ newIORef []
  dflags <- getSessionDynFlags
  let logAction :: LogAction
      logAction _ _ sev loc _ msg
        | SevFatal <- sev = log
        | SevError <- sev = log
        | otherwise = reject
        where
          log = modifyIORef msg_ref ((locstr ++ " " ++ showSafe dflags msg) :)
          locstr = showSafe dflags loc
          reject = return ()
  void $ setSessionDynFlags dflags {log_action = logAction}
  res <- defaultErrorHandler (\err -> modifyIORef msg_ref (err :)) (FlushOut (return ())) act
  dflags' <- getSessionDynFlags
  void $ setSessionDynFlags dflags' {log_action = log_action dflags}
  msgs <- reverse <$> liftIO (readIORef msg_ref)
  return (res, msgs)