{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : Endemic.Repair
-- Description : Replaces holes in functions with other found expressions.
-- License     : MIT
-- Stability   : experimental
--
-- This is a highlevel module that utilizes most of the other modules implemented.
-- This is an impure module, as GHC is run which requires IO.
--
-- Abbreviations:
--    nzh=non-zero-holes - Holes that are touched by properties
--    id_prog=identity-Program - The unchanged Program (initial input to repair)
--
-- Note on (SrcSpan, LHsExpr GhcPs):
-- We thought about Synomising this, but it resembles multiple things;
--   1. An expression and it's (new) hole
--   2. An expression and it's (possible) fix/patch (where fix means candidate, not
--      definitely solved)
module Endemic.Repair where

import Bag (bagToList, emptyBag, listToBag)
import Constraint
import Control.Arrow (first, second, (***))
import Control.Concurrent (getNumCapabilities, threadDelay, threadWaitRead)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_, void, when, (>=>))
import qualified Data.Bifunctor
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.Char (isAlphaNum)
import Data.Default
import Data.Dynamic (fromDyn)
import Data.Either (lefts)
import Data.Function (on)
import Data.Functor (($>))
import qualified Data.Functor
import Data.IORef (IORef, modifyIORef', writeIORef)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (groupBy, intercalate, nub, nubBy, partition, sort, sortOn, transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Data.Tree (flatten)
import Desugar (deSugarExpr)
import Endemic.Check
import Endemic.Configuration
import Endemic.Eval
import Endemic.Plugin (HoleFitState, resetHoleFitCache, resetHoleFitList)
import Endemic.Traversals (fillHole, flattenExpr, replaceExpr, sanctifyExpr, wrapExpr)
import Endemic.Types
import Endemic.Util
import FV (fvVarSet)
import GHC
import GHC.Prim (unsafeCoerce#)
import GhcPlugins
import Numeric (showHex)
import PrelNames (mkMainModule, mkMainModule_)
import StringBuffer (stringToStringBuffer)
import System.Directory (createDirectoryIfMissing, doesFileExist, removeDirectory, removeDirectoryRecursive, removeFile)
import System.Exit (exitFailure)
import System.FilePath (dropExtension, dropFileName, takeDirectory, takeFileName, (<.>), (</>))
import System.IO (Handle, IOMode (ReadMode, ReadWriteMode, WriteMode), hClose, hFlush, hGetLine, openTempFile)
import System.Posix (fdToHandle, handleToFd)
import System.Posix.Process
import System.Posix.Signals
import System.Process
import qualified System.Timeout (timeout)
import TcHoleErrors (HoleFit (..))
import TcSimplify (captureTopConstraints)
import Text.Read (readMaybe)

-- | Provides a GHC without a ic_default set.
-- IC is short for Interactive-Context (ic_default is set to empty list).
setNoDefaulting :: Ghc ()
setNoDefaulting = do
  -- Make sure we don't do too much defaulting by setting `default ()`
  -- Note: I think this only applies to the error we would be generating,
  -- I think if we replace the UnboundVar with a suitable var of the right
  -- it would work... it just makes the in-between output a bit confusing.
  env <- getSession
  setSession (env {hsc_IC = (hsc_IC env) {ic_default = Just []}})

-- | Runs the whole compiler chain to get the fits for a hole, i.e. possible
-- replacement-elements for the holes
getHoleFits ::
  CompileConfig ->
  -- |  A given Compiler Config
  [ExprFitCand] ->
  -- | A list of existing and reachable candidate-expression
  [([SrcSpan], LHsExpr GhcPs)] ->
  -- | The existing Expressions including holes
  Ghc [[[HsExpr GhcPs]]]
getHoleFits cc local_exprs exprs = initGhcCtxt' True cc local_exprs >>= flip (getHoleFits' cc) exprs

-- Gets the holes without any initialization, provided a given plugin
getHoleFits' ::
  CompileConfig ->
  IORef HoleFitState ->
  [([SrcSpan], LHsExpr GhcPs)] ->
  Ghc [[[HsExpr GhcPs]]]
getHoleFits' _ _ [] = return []
getHoleFits' cc@CompConf {..} plugRef exprs = do
  -- Then we can actually run the program!
  setNoDefaulting
  res <- getHoleFits' plugRef (if holeLvl == 0 then 0 else holeDepth) exprs
  return $ map (map (map snd . snd) . snd) res
  where
    exprFits plugRef expr =
      liftIO (modifyIORef' plugRef resetHoleFitList)
        >> handleSourceError
          (getHoleFitsFromError plugRef)
          (Right <$> compileParsedExpr expr)
    getHoleFits' ::
      IORef HoleFitState ->
      Int ->
      [([SrcSpan], LHsExpr GhcPs)] ->
      Ghc [(LHsExpr GhcPs, [(SrcSpan, [(Int, HsExpr GhcPs)])])]
    getHoleFits' plugRef n exprs | n <= 0 = do
      -- We don't want any more additional hole fits, so we just grab the
      -- identifier ones.
      -- We know we won't be lookning at the refinement hole-fits,
      -- so we ignore them:
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags {refLevelHoleFits = Just 0}
      res <-
        mapM (\(l, e) -> mapLeft ((e,) . zip l) <$> exprFits plugRef e) exprs
          >>= processFits . map (second (map (second fst))) . lefts
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags {refLevelHoleFits = Just 0}
      return res
    getHoleFits' plugRef n exprs = do
      fits <- lefts <$> mapM (\(l, e) -> mapLeft ((e,) . zip l) <$> exprFits plugRef e) exprs
      procs <- processFits $ map (second $ map (second (uncurry (++)))) fits
      --       pprPanic "ppr" $ ppr procs
      let f :: LHsExpr GhcPs -> SrcSpan -> [(Int, HsExpr GhcPs)] -> Ghc (SrcSpan, [(Int, HsExpr GhcPs)])
          f expr loc fits = do
            let (hasHoles, done) = partition ((> 0) . fst) fits
                filled =
                  mapMaybe
                    ( \(lvl, fit) ->
                        (fmap ((fit,) . first (replicate lvl)) . (`fillHole` expr)) fit
                    )
                    hasHoles

            recur <- zip (map (L loc . fst) filled) . map snd <$> getHoleFits' plugRef (n -1) (map snd filled)
            let repls = map (second (reverse . map (map snd . snd))) recur >>= uncurry replacements
                procced :: [(Int, HsExpr GhcPs)]
                procced = map ((0,) . unLoc . snd) repls
            return (loc, done ++ procced)
      mapM (\(e, xs) -> (e,) <$> mapM (uncurry (f e)) xs) procs

    mapLeft :: (a -> c) -> Either a b -> Either c b
    mapLeft f (Left a) = Left (f a)
    mapLeft _ (Right r) = Right r

-- | Takes a list of list of list of hole fits and processes each fit so that
-- it becomes a proper HsExpr
processFits ::
  [(LHsExpr GhcPs, [(SrcSpan, [HoleFit])])] ->
  Ghc [(LHsExpr GhcPs, [(SrcSpan, [(Int, HsExpr GhcPs)])])]
processFits fits = do
  -- We process the fits ourselves, since we might have some expression fits
  let processFit :: SrcSpan -> HoleFit -> Ghc (Int, HsExpr GhcPs)
      processFit loc HoleFit {..} = return (hfRefLvl, addPar $ app hfRefLvl)
        where
          addPar
            | hfRefLvl > 0 = HsPar NoExtField . L loc
            | otherwise = id
          app :: Int -> HsExpr GhcPs
          app 0 =
            HsVar noExtField $ noLoc (nukeExact $ getName hfId)
          app n = HsApp NoExtField (noLoc $ app (n - 1)) $ noLoc hv
            where
              hn = show n
              hv = HsUnboundVar NoExtField (TrueExprHole $ mkVarOcc $ "_" ++ locToStr loc ++ "_" ++ hn)
              locToStr (UnhelpfulSpan x) = unpackFS x
              locToStr s@(RealSrcSpan r) =
                intercalate "_" $
                  map show $
                    [srcSpanStartLine r, srcSpanStartCol r]
                      ++ ([srcSpanEndLine r | not (isOneLineSpan s)])
                      ++ [srcSpanEndCol r]
      processFit loc (RawHoleFit sd) = do
        (L l e) <- parseExprNoInit (showUnsafe sd)
        return (0, unLoc $ parenthesizeHsExpr appPrec (L loc e))
      -- NukeExact copied from RdrName
      nukeExact :: Name -> RdrName
      nukeExact n
        | isExternalName n = Orig (nameModule n) (nameOccName n)
        | otherwise = Unqual (nameOccName n)
  mapM (\(e, xs) -> (e,) <$> mapM (\(l, ffh) -> (l,) <$> mapM (processFit l) ffh) xs) fits

-- |  Takes an expression with one or more holes and a list of expressions that
-- fit each holes and returns a list of expressions where each hole has been
-- replaced with a candidate, which candidate it was and where in the expression.
replacements ::
  -- | The original expression with one or more holes
  LHsExpr GhcPs ->
  -- | A list of expressions that fits the holes
  [[HsExpr GhcPs]] ->
  [([(SrcSpan, HsExpr GhcPs)], LHsExpr GhcPs)]
replacements e [] = [([], e)]
replacements e (first_hole_fit : rest) = concat rest_fit_res
  where
    -- mapMaybe', but keep the result
    mapMaybe' :: (a -> Maybe b) -> [a] -> [(a, b)]
    mapMaybe' _ [] = []
    mapMaybe' f (a : as) = (case f a of Just b -> ((a, b) :); _ -> id) $ mapMaybe' f as
    res =
      map (\(e', (l, r)) -> ([(l, e')], r)) $
        mapMaybe' (`fillHole` e) first_hole_fit
    (first_fit_locs_and_e, first_fit_res) = unzip res
    rest_fit_res = zipWith addL first_fit_locs_and_e $ map (`replacements` rest) first_fit_res
    -- addL = Add Left, adds the gien expression at any lefthand side of the expressions existing
    addL ::
      [(SrcSpan, HsExpr GhcPs)] ->
      [([(SrcSpan, HsExpr GhcPs)], LHsExpr GhcPs)] ->
      [([(SrcSpan, HsExpr GhcPs)], LHsExpr GhcPs)]
    addL srcs reses = map (first (srcs ++)) reses

-- | Translate from the old String based version to the new LHsExpr version.
translate :: HasCallStack => CompileConfig -> RProblem -> IO EProblem
translate cc RProb {..} = runGhc' cc $ do
  e_prog' <- parseExprNoInit r_prog
  ~(L _ (ExprWithTySig _ _ e_ty)) <- parseExprNoInit ("undefined :: " ++ r_ty)
  let clt = "let {" ++ (intercalate "; " . concatMap lines $ r_ctxt) ++ "} in undefined"
  ~(L _ (HsLet _ e_ctxt _)) <- parseExprNoInit clt
  let plt = "let {" ++ (intercalate "; " . concatMap lines $ r_props) ++ "} in undefined"
  ~(L _ (HsLet _ (L _ (HsValBinds _ (ValBinds _ lbs _))) _)) <- parseExprNoInit plt
  let e_props = bagToList lbs
      e_target = mkVarUnqual $ fsLit r_target
      e_prog = [(e_target, e_ty, e_prog')]
      e_module = Nothing
  return (EProb {..})

detranslate :: HasCallStack => EProblem -> RProblem
detranslate EProb {..} =
  let (e_target, e_prog', e_ty) : _ = e_prog
      r_prog = showUnsafe e_prog'
      r_ty = showUnsafe e_ty
      r_props = map showUnsafe e_props
      r_target = showUnsafe e_target
      (L _ (HsValBinds _ (ValBinds _ bs sigs))) = e_ctxt
      r_ctxt = map showUnsafe sigs ++ map showUnsafe (bagToList bs)
   in RProb {..}
detranslate _ = error "Cannot detranlsate external problem!"

-- | Get a list of strings which represent shrunk arguments to the property that
-- makes it fail.
propCounterExample :: CompileConfig -> EProblem -> EProp -> IO (Maybe [RExpr])
propCounterExample cc ep prop = head <$> propCounterExamples (fakeDesc [] cc ep) [prop]

-- | Get a list of strings which represent shrunk arguments to the property that
-- makes it fail.
propCounterExamples :: ProblemDescription -> [EProp] -> IO [Maybe [RExpr]]
propCounterExamples ProbDesc {..} props = runGhc' cc' $ do
  let mk_bcc prop seed = buildCounterExampleCheck compConf seed prop progProblem
      checkProp prop | isTastyProp prop = return $ Just []
      checkProp prop = do
        seed <- liftIO newSeed
        exec <- dynCompileParsedExpr `reportOnError` mk_bcc prop seed
        (map addPar <$>) <$> liftIO (fromDyn exec (return Nothing))
  mapM checkProp props
  where
    cc' = (compConf {importStmts = checkImports ++ importStmts compConf})
    addPar arg = ('(' : arg) ++ ")"
    -- TODO: If we had the type here as well, we could do better.
    isTastyProp :: LHsBind GhcPs -> Bool
    isTastyProp (L _ FunBind {fun_id = fid}) =
      "prop" /= take 4 (occNameString $ rdrNameOcc $ unLoc fid)
    isTastyProp _ = True

-- | Returns the props that are failing given a problem description.
failingProps' :: ProblemDescription -> Ghc [EProp]
failingProps' desc@ProbDesc {progProblem = EProb {..}, ..} = do
  ~[res] <- liftIO $ checkFixes desc [eProgToEProgFixAtTy e_prog]
  return $ case res of
    Right True -> []
    Right False -> e_props
    Left results -> map fst $ filter (not . snd) $ zip e_props results
failingProps' _ = error "External fixes not supported!"

-- | Returns the props that fail for the given program, without having a
-- proper description. DO NOT USE IF YOU HAVE A DESCRIPTION
failingProps :: CompileConfig -> EProblem -> IO [EProp]
failingProps cc prob = runGhc' cc $ failingProps' (fakeDesc [] cc prob)

fakeDesc :: [ExprFitCand] -> CompileConfig -> EProblem -> ProblemDescription
fakeDesc efcs cc prob =
  ProbDesc
    { progProblem = prob,
      exprFitCands = efcs,
      compConf = cc,
      probModule = Nothing,
      initialFixes = Nothing,
      addConf = def {assumeNoLoops = True}
    }

-- | Primary method of this module.
-- It takes a program & configuration,
-- a (translated) repair problem and returns a list of potential fixes.
repair :: CompileConfig -> EProblem -> IO [EFix]
repair cc prob@EProb {..} = do
  ecfs <- runGhc' cc $ getExprFitCands $ Left $ noLoc $ HsLet NoExtField e_ctxt $ noLoc undefVar
  let desc@ProbDesc {..} = fakeDesc ecfs cc prob
  map fst . filter (isFixed . snd) <$> repairAttempt desc
repair _ _ = error "Cannot repair external problems yet!"

-- | Finds the locations in the program that are evaluated by failing tests
-- and returns those as programs with holes at that location.
findEvaluatedHoles ::
  ProblemDescription ->
  IO [(SrcSpan, LHsExpr GhcPs)]
findEvaluatedHoles
  desc@ProbDesc
    { compConf = cc,
      progProblem = tp@EProb {..}
    } = runGhc' cc $ do
    liftIO $ logStr DEBUG "Finding evaluated holes..."
    -- We apply the fixes to all of the contexts, and all of the contexts
    -- contain the entire current program.
    -- TODO: Is this safe?
    let id_prog = eProgToEProgFixAtTy e_prog

    liftIO $ logStr DEBUG "Building trace correlation..."
    trace_correl <- buildTraceCorrel cc tp id_prog

    -- We can use the failing_props and the counter_examples to filter
    -- out locations that we know won't matter.
    liftIO $ logStr DEBUG "Finding failing props..."
    failing_props <- collectStats $ failingProps' desc

    liftIO $ logStr DEBUG "Finding counter examples..."
    counter_examples <- liftIO $ collectStats $ propCounterExamples desc failing_props

    let hasCE (p, Just ce) = Just (p, ce)
        hasCE _ = Nothing
        -- ps_w_ce = Properties with CounterExamples -> The Failing Properties
        ps_w_ce = mapMaybe hasCE $ zip failing_props counter_examples
    -- We compute the locations that are touched by the failing counter-examples
    liftIO $ logStr DEBUG "Tracing program..."
    -- This Method helps us to go resolve the traces per expressions touched by properties
    -- To get the traces per properties touched by expressions.
    -- If it were a matrix, this would be a classic matrix transpose.
    -- We introduce the Ints before Properties and EExprs to have a trace-able ID for them,
    -- As they do not provide Equality themselves.
    -- However, (Toplevel) Expressions and Properties are unique, so we do not carry around duplicates.
    -- It was just easier to use an Integer as a helper than to implement equality for Compiler-Objects.
    let assigToExprProp :: [((Integer, EProp), [((Int, EExpr), Trace)])] -> [(EExpr, [(EProp, Trace)])]
        assigToExprProp xs = resolve $ joinExprs mergeExprs
          where
            eprop_map :: Map Integer EProp
            eprop_map = Map.fromList (map fst xs)
            expr_map :: IntMap EExpr
            expr_map = IntMap.fromList $ map fst $ concatMap snd xs
            indsOnly :: [(Integer, [(Int, Trace)])]
            indsOnly = map (Data.Bifunctor.bimap fst (map (first fst))) xs
            dupProps :: [(Integer, (Int, Trace))]
            dupProps = concatMap (\(p, ls) -> map (p,) ls) indsOnly
            flipProps :: [(Int, (Integer, Trace))]
            flipProps = map (\(p, (e, t)) -> (e, (p, t))) dupProps
            mergeExprs :: [[(Int, (Integer, Trace))]]
            mergeExprs = groupBy ((==) `on` fst) $ sortOn fst flipProps
            joinExprs :: [[(Int, (Integer, Trace))]] -> [(Int, [(Integer, Trace)])]
            joinExprs xs = map comb xs
              where
                comb :: [(Int, (Integer, Trace))] -> (Int, [(Integer, Trace)])
                comb xs@((i, _) : _) = foldr f (i, []) xs
                  where
                    f :: (Int, (Integer, Trace)) -> (Int, [(Integer, Trace)]) -> (Int, [(Integer, Trace)])
                    f (_, t) (i, ts) = (i, t : ts)
                comb [] = error "expr with no traces!"
            resolve :: [(Int, [(Integer, Trace)])] -> [(EExpr, [(EProp, Trace)])]
            resolve = map $ (expr_map IntMap.!) *** map (first (eprop_map Map.!))

    -- traces that worked are all non-empty traces that are sufficiently mapped to exprs and props
    traces_that_worked <-
      liftIO $
        map (second (Map.unionsWith (+) . map (toInvokes . snd)))
          -- The traces are per prop per expr, but we need a per expr per prop,
          -- so we add indices and then use this custom transpose to get
          -- the traces per expr. (See above)
          . assigToExprProp
          . map (zip (zip [0 :: Int ..] id_prog) <$>)
          . mapMaybe (\((i, (p, _)), tr) -> ((i, p),) <$> tr)
          . zip (zip [0 :: Integer ..] ps_w_ce)
          <$> traceTargets cc tp id_prog ps_w_ce

    -- We then remove suggested holes that are unlikely to help (naively for now
    -- in the sense that we remove only holes which did not get evaluated at all,
    -- so they are definitely not going to matter).
    let fk trace_correl (expr, invokes) =
          filter ((`Set.member` non_zero_src) . fst) $ sanctifyExpr expr
          where
            non_zero = filter ((> 0) . snd) $ Map.toList invokes
            non_zero_src = Set.fromList $ mapMaybe ((trace_correl Map.!?) . fst) non_zero
        non_zero_holes = zipWith fk trace_correl traces_that_worked
        -- nubOrd deduplicates collections of sortable items. it's faster than other dedups.
        nubOrd = Set.toList . Set.fromList
    return $ nubOrd $ concat non_zero_holes
findEvaluatedHoles _ = error "Cannot find evaluated holes of external problems yet!"

-- | This method tries to repair a given Problem.
-- It first creates the program with holes in it and runs it against the properties.
-- From this, candidates are retrieved of touched holes and fixes are created and run.
-- Quite some information can be printed when the program is run in DEBUG.
-- As an important sidenote, places that are not in failing properties will not be altered.
repairAttempt ::
  ProblemDescription ->
  IO [(EFix, TestSuiteResult)]
repairAttempt desc@ProbDesc {compConf = cc, ..} = do
  let desc' = desc {addConf = addConf {assumeNoLoops = False}}
  nzh <- findEvaluatedHoles desc'
  (efixes, efixprogs) <- unzip <$> generateFixCandidates desc nzh
  zip efixes <$> liftIO (checkFixes desc' efixprogs)

-- | Retrieves the candidates for the holes given in non-zero holes
generateFixCandidates ::
  ProblemDescription ->
  [(SrcSpan, LHsExpr GhcPs)] ->
  IO [(EFix, EProgFix)]
generateFixCandidates
  desc@ProbDesc
    { compConf = cc,
      progProblem = tp@EProb {..},
      exprFitCands = efcs
    }
  non_zero_holes = runGhcWithCleanup cc $ do
    -- We add the context by replacing a hole in a let.
    let nzh = non_zero_holes
        inContext = noLoc . HsLet NoExtField e_ctxt
        addContext :: SrcSpan -> LHsExpr GhcPs -> LHsExpr GhcPs
        addContext l = snd . fromJust . flip fillHole (inContext $ L l hole) . unLoc
        holed_exprs = map (\(l, he) -> ([l], addContext l he)) nzh
        wrapInHole loc hsexpr = HsPar NoExtField (noLoc $ HsApp NoExtField (L loc hole) (noLoc hsexpr))
        ((_, _, one_prog) : _) = e_prog
        wrapped_in_holes =
          if allowFunctionFits cc
            then map ((\l -> ([l], wrapExpr l (wrapInHole l) one_prog)) . fst) nzh
            else []
    plugRef <- initGhcCtxt' True cc efcs
    hole_fits <- collectStats $ getHoleFits' cc plugRef holed_exprs
    raw_wrapped_fits <- collectStats $ getHoleFits' cc plugRef wrapped_in_holes
    let subexprs = Map.fromList (map (\(L l k) -> (l, k)) $ flattenExpr one_prog)
        with_wrappee :: [[[HsExpr GhcPs]]]
        with_wrappee =
          zipWith
            ( \wrp fits ->
                map
                  ( map
                      ( \fit ->
                          HsPar NoExtField $
                            noLoc $
                              HsApp NoExtField (noLoc fit) $
                                noLoc $
                                  HsPar NoExtField $ noLoc wrp
                      )
                  )
                  fits
            )
            (map ((subexprs Map.!) . fst) nzh)
            raw_wrapped_fits
        wrapped_holes = map (first head) wrapped_in_holes

    let fix_cands' :: [EFix]
        fix_cands' = concatMap toCands $ zip (nzh ++ wrapped_holes) (hole_fits ++ with_wrappee)
          where
            toCands ((loc, hole_expr), [fits])
              | isGoodSrcSpan loc =
                map (Map.singleton loc) $ nubSort fits
            -- We ignore the spans than are bad or unhelpful.
            toCands ((loc, _), [_]) = []
            toCands ((_, hole_expr), multi_fits) =
              map (Map.fromList . fst) $ replacements hole_expr multi_fits
        fix_cands :: [(EFix, EProgFix)]
        fix_cands = map (\f -> (f,) $ map (\(_, _, prog) -> f `replaceExpr` prog) e_prog) fix_cands'
    liftIO $ logStr DEBUG "Fix candidates:"
    liftIO $ mapM_ (logOut DEBUG) fix_cands
    liftIO $ logStr DEBUG "Those were all of them!"
    return fix_cands
generateFixCandidates _ _ = error "External problems not supported"

-- | Runs a given (changed) Program against the Test-Suite described in ProblemDescription.
-- The Result is the Test-Suite-Result, where
-- Right True is full success (the tests exited with 0), Right False is full failure (e.g., timeout or errors in the test-framework)
-- Left [Bool] expresses a list of the results of each run test, where true is passing and false is failing.
-- Shortwires an empty list when the EExpr list is empty.
-- TODO: DOCUMENT (Further)
checkFixes ::
  ProblemDescription ->
  [EProgFix] ->
  IO [TestSuiteResult]
checkFixes _ [] = return []
checkFixes
  ProbDesc
    { compConf = cc@CompConf {..},
      progProblem = tp,
      addConf = AddConf {..},
      ..
    }
  -- Because checkFixes sets the dynFlags, we need to run it in a separate
  -- thread: hence the runGhc' here
  fixes = runGhc' cc $ do
    liftIO $ logStr DEBUG "Checking fixes..."
    seed <- liftIO newSeed
    let checkHash = flip showHex "" $ abs $ hashString $ showSDocUnsafe $ ppr (tp, fixes, seed)
        tempDir = tempDirBase </> "checks" </> checkHash
        the_f = tempDir </> ("FakeCheckTarget" ++ checkHash) <.> "hs"
    liftIO $ createDirectoryIfMissing True tempDir
    -- We generate the name of the module from the temporary file
    let mname = filter isAlphaNum $ dropExtension $ takeFileName the_f
        modTxt = exprToCheckModule cc seed mname tp fixes
        exeName = dropExtension the_f
        timeoutVal = fromIntegral timeout

    liftIO $ logStr DEBUG modTxt
    -- Note: we do not need to dump the text of the module into the file, it
    -- only needs to exist. Otherwise we would have to write something like
    -- `hPutStr handle modTxt`
    liftIO $ writeFile the_f modTxt
    liftIO $ mapM_ (logStr DEBUG) $ lines modTxt
    let shouldInterpret = useInterpreted && assumeNoLoops
    dynFlags <- getSessionDynFlags
    _ <-
      setSessionDynFlags $
        -- turn-off all warnings
        flip (foldl wopt_unset) [toEnum 0 ..] $
          flip (foldl gopt_unset) setFlags $ -- Remove the HPC
            dynFlags
              { hpcDir = tempDir,
                -- ghcMode = OneShot is the default, if we want it to compile
                -- the files. But we've already compiled them at this point,
                -- so we want it to use the CompManager to pick up the
                -- dependencies.
                ghcMode = CompManager,
                -- TODO: Should this be LinkDynLib for MacOS?
                ghcLink = if shouldInterpret then LinkInMemory else LinkBinary,
                hscTarget = if shouldInterpret then HscInterpreted else HscAsm,
                optLevel = if shouldInterpret then 0 else 2
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
    liftIO $ logStr DEBUG $ "Loading up to " ++ moduleNameString target_name
    sf2 <- load (LoadUpTo target_name)
    when (failed sf2) $
      liftIO $ do
        logStr ERROR $
          "Error while loading: " ++ moduleNameString target_name
            ++ " see error message for more information"

        exitFailure
    mg <- depanal [] True
    liftIO $ logStr DEBUG "New graph:"
    liftIO $ mapM (logOut DEBUG) $ mgModSummaries mg

    let p '1' = Just True
        p '0' = Just False
        p _ = Nothing
        toP True = '1'
        toP _ = '0'
        startCheck :: Int -> IO (Handle, ProcessHandle)
        startCheck which = do
          (_, Just hout, _, ph) <- do
            let tixFilePath = exeName ++ "_" ++ show which ++ ".tix"
            logStr DEBUG $ "Checking " ++ show which ++ ":"
            logOut DEBUG (fixes !! which)
            createProcess
              (proc exeName [show which])
                { -- TODO: /dev/null should be NUL if we're on windows.
                  env = Just [("HPCTIXFILE", tixFilePath)],
                  -- We ignore the output
                  std_out = CreatePipe
                }
          return (hout, ph)
        waitOnCheck :: (Handle, ProcessHandle) -> IO TestSuiteResult
        waitOnCheck (hout, ph) = do
          ec <- System.Timeout.timeout timeoutVal $ waitForProcess ph
          case ec of
            Nothing -> do
              terminateProcess ph
              getPid ph >>= maybe (return ()) (signalProcess sigKILL)
              return (Right False)
            Just _ -> do
              res <- hGetLine hout
              hClose hout
              let parsed = mapMaybe p res
              return $
                if length parsed == length res
                  then if and parsed then Right True else Left parsed
                  else Right False

    let inds = take (length fixes) [0 ..]
        m_name = mkModuleName mname
        checkArr arr = if and arr then Right True else Left arr
        forkWithFile :: ([Bool] -> Either [Bool] Bool) -> IO [Bool] -> IO (Either [Bool] Bool)
        forkWithFile trans a =
          maybe (Right False) trans <$> runInProc (2 * timeoutVal) encode decode a
          where
            encode = BSC.pack . map toP
            decode = Just . mapMaybe p . BSC.unpack
        fwf = if assumeNoLoops then fmap else forkWithFile
        toB (Right False) = "R0"
        toB (Right True) = "R1"
        toB (Left xs) = "L" ++ map toP xs
        isRorL c = (c == 'L') || (c == 'R')
        fromB ['R', x] | Just b <- p x = Just $ Right b
        fromB ('L' : xs) = Just $ Left $ mapMaybe p xs
        fromB _ = Nothing
        splitRes [] = []
        splitRes xs =
          f : case r of
            [] -> []
            (rorl : rest) ->
              let (r, g) = break isRorL rest
               in (rorl : r) : splitRes rest
          where
            (f, r) = break isRorL xs
    batchSize <- liftIO getNumCapabilities
    let interpretLoop checks = do
          logStr DEBUG "Running checks..."
          res <-
            if parChecks
              then do
                -- We want the process to die in case we were wrong,
                -- better than hanging.
                scheduleAlarm (1 + 10 * length checks * ceiling (fromIntegral timeoutVal / 1_000_000))
                howToRun (evf evalCheck checks)
                  >>= (<$ scheduleAlarm 0) -- We finished, so turn off the alarm
                  >>= \case
                    Just res ->
                      (res <$) $
                        when (length res /= length checks) $ do
                          logStr ERROR "Fewer results received than expected!"
                          logStr ERROR ("Expected " ++ show (length checks))
                          logStr ERROR ("Got " ++ show (length res))
                          exitFailure
                    Nothing -> mapM (forkWithFile checkArr) checks
              else evf evalCheck checks
          logStr DEBUG "Done checking!"
          return res
          where
            encode = BSC.pack . concatMap toB
            decode = Just . mapMaybe fromB . splitRes . BSC.unpack
            (evalCheck, evf) =
              if parChecks
                then ((checkArr <$>), mapConcurrently)
                else (fwf checkArr, mapM)
            howToRun =
              if assumeNoLoops
                then (Just <$>)
                else runInProc ((1 + length checks) * timeoutVal) encode decode
        nonInterpretLoop inds =
          if parChecks
            then do
              -- By starting all the processes and then waiting on them, we get more
              -- mode parallelism.
              liftIO $ logStr DEBUG "Running checks..."
              procs <- collectStats $ mapM startCheck inds
              collectStats $ mapM waitOnCheck procs
            else collectStats $ mapM (startCheck >=> waitOnCheck) inds
    res <-
      concat
        <$> if shouldInterpret
          then do
            liftIO $ logStr DEBUG $ "Adding " ++ mname
            setContext [IIDecl $ simpleImportDecl m_name]
            liftIO $ logStr DEBUG "Compiling checks__"
            checks_expr <- compileExpr "checks__"
            let checks :: [IO [Bool]]
                checks = unsafeCoerce# checks_expr
            liftIO $ mapM interpretLoop $ chunkList batchSize checks
          else liftIO $ mapM nonInterpretLoop $ chunkList batchSize inds
    cleanupAfterLoads tempDir mname dynFlags
    return res

describeProblem :: Configuration -> FilePath -> IO (Maybe ProblemDescription)
describeProblem conf@Conf {compileConfig = ogcc} fp = do
  logStr DEBUG "Describing problem..."
  (compConf@CompConf {..}, modul, problem) <- moduleToProb ogcc fp Nothing
  case problem of
    Just ExProb {} -> error "External targets not supported!"
    Nothing -> return Nothing
    Just progProblem@EProb {..} ->
      Just <$> do
        exprFitCands <- runGhc' compConf $ getExprFitCands $ Right modul
        let probModule = Just modul
            initialFixes = Nothing
            addConf = AddConf {assumeNoLoops = True}
            descBase = ProbDesc {..}
        if precomputeFixes
          then do
            logStr DEBUG "Pre-computing fixes..."
            let desc' = descBase {addConf = addConf {assumeNoLoops = False}}
                fix_str_length :: EFix -> Int
                fix_str_length = sum . map (length . showSDocUnsafe . ppr) . Map.elems
                fitness :: TestSuiteResult -> Float
                fitness (Right True) = 0
                fitness (Right False) = 1
                fitness (Left r) = 1 - avg (map (realToFrac . fromEnum) r)
            (t, initialFixes') <- time $
              -- We sort the initialFixes by a very basic estimation of
              -- how far from the result they are
              collectStats $ do
                if keepLoopingFixes
                  then
                    sortOn fix_str_length
                      . map fst
                      <$> (findEvaluatedHoles desc' >>= generateFixCandidates desc')
                  else do
                    concatMap (sortOn fix_str_length . map snd)
                      . groupBy ((==) `on` fst)
                      . sortOn fst
                      . map (\(f, r) -> (fitness r, f))
                      . filter ((/= Right False) . snd)
                      <$> repairAttempt desc'
            logStr DEBUG $ "Found  " ++ show (length initialFixes) ++ " initial fixes:"
            mapM_ (logOut DEBUG) initialFixes'
            return $ descBase {initialFixes = Just initialFixes'}
          else return descBase
