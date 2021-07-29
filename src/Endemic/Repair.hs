{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Endemic.Repair
-- Description : Replaces holes in functions with other found expressions.
-- License     : MIT
-- Stability   : experimental
--
-- This is a highlevel module that utilizes most of the other modules implemented.
-- This is an impure module, as GHC is run which requires IO.
--
-- Note on (SrcSpan, LHsExpr GhcPs):
-- We thought about Synomising this, but it resembles multiple things;
--   1. An expression and it's (new) hole
--   2. An expression and it's (possible) fix/patch (where fix means candidate, not
--      definitely solved)
module Endemic.Repair where

import Bag (bagToList, emptyBag, listToBag)
import Constraint
import Control.Arrow (first, second)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (when, (>=>))
import Data.Char (isAlphaNum)
import Data.Dynamic (fromDyn)
import Data.Either (lefts)
import Data.Function (on)
import Data.IORef (modifyIORef', writeIORef)
import Data.List (intercalate, nub, nubBy, sort, sortOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Data.Tree (flatten)
import Desugar (deSugarExpr)
import Endemic.Check
import Endemic.Configuration
import Endemic.Eval
import Endemic.Plugin (resetHoleFitCache, resetHoleFitList)
import Endemic.Traversals (fillHole, flattenExpr, sanctifyExpr)
import Endemic.Types
import Endemic.Util
import FV (fvVarSet)
import GHC
import GHC.Paths (libdir)
import GHC.Prim (unsafeCoerce#)
import GhcPlugins
import PrelNames (mkMainModule)
import StringBuffer (stringToStringBuffer)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (dropExtension, dropFileName, takeFileName)
import System.IO (Handle, hClose, hGetLine, openTempFile)
import System.Posix.Process
import System.Posix.Signals
import System.Process
import System.Timeout (timeout)
import TcHoleErrors (HoleFit (..))
import TcSimplify (captureTopConstraints)

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
  [LHsExpr GhcPs] ->
  -- | The existing Expressions including holes
  Ghc [[[HsExpr GhcPs]]]
getHoleFits cc local_exprs exprs = do
  plugRef <- initGhcCtxt' True cc local_exprs
  -- Then we can actually run the program!
  setNoDefaulting
  let exprFits expr =
        liftIO (modifyIORef' plugRef resetHoleFitList)
          >> handleSourceError
            (getHoleFitsFromError plugRef)
            (Right <$> compileParsedExpr expr)
  mapM exprFits exprs >>= processFits . map (map fst) . lefts

-- | Returns for a given compiler config and an expression the possible holes to fill.
getHoley ::
  -- | A compiler setup/config to evaluate the expression in
  CompileConfig ->
  -- | A given Expression
  RExpr ->
  -- | All versions of the Expression with new holes poked in it
  IO [(SrcSpan, LHsExpr GhcPs)]
getHoley cc str = runGhc (Just libdir) $ sanctifyExpr <$> justParseExpr cc str

-- |  Takes an expression with one or more holes and a list of expressions that
-- fit each holes and returns a list of expressions where each hole has been
-- replaced with a candidate, which candidate it was and where in the expression.
replacements ::
  -- | The original expression with one or more holes
  LHsExpr GhcPs ->
  -- | A list of expressions that fits the holes
  [[HsExpr GhcPs]] ->
  [([(SrcSpan, HsExpr GhcPs)], LHsExpr GhcPs)]
replacements r [] = [([], r)]
replacements e (first_hole_fit : rest) = concat rest_fit_res
  where
    -- mapMaybe', but keep the result
    mapMaybe' :: (a -> Maybe b) -> [a] -> [(a, b)]
    mapMaybe' _ [] = []
    mapMaybe' f (a : as) = (case f a of Just b -> ((a, b) :); _ -> id) $ mapMaybe' f as
    res = map (\(e', (l, r)) -> ([(l, e')], r)) (mapMaybe' (`fillHole` e) first_hole_fit)
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
translate cc RProb {..} = runGhc (Just libdir) $ do
  _ <- initGhcCtxt cc
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
propCounterExample :: RepairConfig -> CompileConfig -> EProblem -> EProp -> IO (Maybe [RExpr])
propCounterExample rc cc ep prop =
  runGhc' cc $ head <$> propCounterExamples (fakeDesc [] cc rc ep) [prop]

-- | Get a list of strings which represent shrunk arguments to the property that
-- makes it fail.
propCounterExamples :: ProblemDescription -> [EProp] -> Ghc [Maybe [RExpr]]
propCounterExamples ProbDesc {..} props = do
  let cc' = (compConf {hole_lvl = 0, importStmts = checkImports ++ importStmts compConf})
      mk_bcc prop seed = buildCounterExampleCheck repConf seed prop progProblem
      checkProp prop | isTastyProp prop = return $ Just []
      checkProp prop = do
        seed <- liftIO newSeed
        exec <- dynCompileParsedExpr `reportOnError` mk_bcc prop seed
        (map addPar <$>) <$> liftIO (fromDyn exec (return Nothing))
  liftIO $
    runGhc (Just libdir) $ do
      _ <- initGhcCtxt cc'
      mapM checkProp props
  where
    addPar arg = ('(' : arg) ++ ")"
    -- TODO: If we had the type here as well, we could do better.
    isTastyProp :: LHsBind GhcPs -> Bool
    isTastyProp (L _ FunBind {fun_id = fid}) =
      "prop" /= take 4 (occNameString $ rdrNameOcc $ unLoc fid)
    isTastyProp _ = True

-- | Returns the props that are failing given a problem description.
failingProps' :: ProblemDescription -> Ghc [EProp]
failingProps' desc@ProbDesc {progProblem = EProb {..}, ..} =
  do
    ~[res] <- checkFixes desc [eProgToEProgFixAtTy e_prog]
    return $ case res of
      Right True -> []
      Right False -> e_props
      Left results -> map fst $ filter (not . snd) $ zip e_props results
failingProps' _ = error "External fixes not supported!"

-- | Returns the props that fail for the given program, without having a
-- proper description. DO NOT USE IF YOU HAVE A DESCRIPTION
failingProps :: RepairConfig -> CompileConfig -> EProblem -> IO [EProp]
failingProps rc cc prob = runGhc' cc $ failingProps' (fakeDesc [] cc rc prob)

fakeDesc :: [ExprFitCand] -> CompileConfig -> RepairConfig -> EProblem -> ProblemDescription
fakeDesc efcs cc rc prob =
  ProbDesc
    { progProblem = prob,
      exprFitCands = efcs,
      compConf = cc,
      repConf = rc,
      probModule = Nothing,
      initialFixes = Nothing
    }

-- | Primary method of this module.
-- It takes a program & configuration,
-- a (translated) repair problem and returns a list of potential fixes.
repair :: CompileConfig -> RepairConfig -> EProblem -> IO [EFix]
repair cc rc prob@EProb {..} = do
  ecfs <- runGhc' cc $ getExprFitCands $ Left $ noLoc $ HsLet NoExtField e_ctxt $ noLoc undefVar
  let desc = fakeDesc ecfs cc rc prob
  map fst . filter (isFixed . snd) <$> repairAttempt desc
repair _ _ _ = error "Cannot repair external problems yet!"

-- | Finds the locations in the program that are evaluated by failing tests
-- and returns those as programs with holes at that location.
findEvaluatedHoles ::
  ProblemDescription ->
  IO [LHsExpr GhcPs]
findEvaluatedHoles
  desc@ProbDesc
    { compConf = cc,
      repConf = rc,
      progProblem = tp@EProb {..}
    } = collectStats $ do
    logStr DEBUG "Finding evaluated holes..."
    -- We apply the fixes to all of the contexts, and all of the contexsts
    -- contain the entire current program.
    -- TODO: Is this safe?
    let id_prog = eProgToEProgFixAtTy e_prog
        holey_exprss = map sanctifyExpr id_prog

    logStr DEBUG "Building trace correlation..."
    trace_correl <- buildTraceCorrel cc tp id_prog

    -- We can use the failing_props and the counter_examples to filter
    -- out locations that we know won't matter.
    logStr DEBUG "Finding failing props..."
    runGhc' cc $ do
      failing_props <- collectStats $ failingProps' desc

      liftIO $ logStr DEBUG "Finding counter examples..."
      counter_examples <- collectStats $ propCounterExamples desc failing_props

      let hasCE (p, Just ce) = Just (p, ce)
          hasCE _ = Nothing
          ps_w_ce = mapMaybe hasCE $ zip failing_props counter_examples
      -- We compute the locations that are touched by the failing counter-examples
      liftIO $ logStr DEBUG "Tracing program..."
      all_invokes <-
        collectStats $
          map
            ( Map.toList
                . Map.unionsWith (+)
                . map toInvokes
            )
            . catMaybes
            <$> liftIO (traceTargets rc cc tp id_prog ps_w_ce)

      -- We then remove suggested holes that are unlikely to help (naively for now
      -- in the sense that we remove only holes which did not get evaluated at all,
      -- so they are definitely not going to matter).
      let fk holey_exprs trace_correl invokes = map snd $ filter ((`Set.member` non_zero_src) . fst) holey_exprs
            where
              non_zero = filter ((> 0) . snd) invokes
              non_zero_src = Set.fromList $ mapMaybe ((trace_correl Map.!?) . fst) non_zero
          non_zero_holes = zipWith3 fk holey_exprss trace_correl all_invokes
          nubOrd = Set.toList . Set.fromList
      return $ nubOrd $ concat non_zero_holes
findEvaluatedHoles _ = error "Cannot find evaluated holes of external problems yet!"

-- | Takes a list of list of list of hole fits and processes each fit so that
-- it becomes a proper HsExpr
processFits :: [[[HoleFit]]] -> Ghc [[[HsExpr GhcPs]]]
processFits fits = do
  -- We process the fits ourselves, since we might have some expression fits
  let processFit :: HoleFit -> Ghc (HsExpr GhcPs)
      processFit HoleFit {..} =
        return $ HsVar noExtField (L noSrcSpan (nukeExact $ getName hfId))
        where
          -- NukeExact copied from RdrName
          nukeExact :: Name -> RdrName
          nukeExact n
            | isExternalName n = Orig (nameModule n) (nameOccName n)
            | otherwise = Unqual (nameOccName n)
      processFit (RawHoleFit sd) =
        unLoc . parenthesizeHsExpr appPrec <$> parseExprNoInit (showUnsafe sd)
  mapM (mapM (mapM processFit)) fits

-- | This method tries to repair a given Problem.
-- It first creates the program with holes in it and runs it against the properties.
-- From this, candidates are retrieved of touched holes and fixes are created and run.
-- Quite some information can be printed when the program is run in DEBUG.
-- As an important sidenote, places that are not in failing properties will not be altered.
repairAttempt ::
  ProblemDescription ->
  IO [(EFix, TestSuiteResult)]
repairAttempt
  desc@ProbDesc
    { compConf = cc,
      repConf = rc,
      progProblem = tp@EProb {..},
      exprFitCands = efcs
    } = collectStats $ do
    -- We add the context by replacing a hole in a let.
    let inContext = noLoc . HsLet NoExtField e_ctxt
        addContext = snd . fromJust . flip fillHole (inContext hole) . unLoc

    nzh <- findEvaluatedHoles desc
    runGhc (Just libdir) $ do
      fits <- collectStats $ getHoleFits cc efcs (map addContext nzh)

      let fix_cands' :: [(EFix, EExpr)]
          fix_cands' =
            -- We do a from and to from a list to avoid duplicates.
            Map.toList $
              Map.fromList $
                map (first Map.fromList) (zip nzh fits >>= uncurry replacements)
          fix_cands :: [(EFix, EProgFix)]
          fix_cands = map (second (replicate (length e_prog))) fix_cands'

      liftIO $ logStr DEBUG "Fix candidates:"
      liftIO $ mapM_ (logOut DEBUG) fix_cands
      liftIO $ logStr DEBUG "Those were all of them!"
      collectStats $
        zipWith (\(fs, _) r -> (fs, r)) fix_cands
          <$> checkFixes desc (map snd fix_cands)
repairAttempt _ = error "Cannot repair external problems yet!"

-- | Runs a given (changed) Program against the Test-Suite described in ProblemDescription.
-- The Result is the Test-Suite-Result, where
-- Right True is full success (the tests exited with 0), Right False is full failure (e.g., timeout or errors in the test-framework)
-- Left [Bool] expresses a list of the results of each run test, where true is passing and false is failing.
-- Shortwires an empty list when the EExpr list is empty.
-- TODO: DOCUMENT (Further)
checkFixes ::
  ProblemDescription ->
  [EProgFix] ->
  Ghc [TestSuiteResult]
checkFixes
  ProbDesc
    { compConf = cc,
      repConf = rc,
      progProblem = tp,
      ..
    }
  fixes = do
    let RepConf {..} = rc
        tempDir = "./fake_targets"
    liftIO $ createDirectoryIfMissing False tempDir
    (the_f, handle) <- liftIO $ openTempFile tempDir "FakeTargetCheck.hs"
    seed <- liftIO $ newSeed
    -- We generate the name of the module from the temporary file
    let mname = filter isAlphaNum $ dropExtension $ takeFileName the_f
        modTxt = exprToCheckModule rc cc seed mname tp fixes
        strBuff = stringToStringBuffer modTxt
        exeName = dropExtension the_f
        timeoutVal = fromIntegral repTimeout
    -- mixFilePath = tempDir

    liftIO $ logStr DEBUG modTxt
    -- Note: we do not need to dump the text of the module into the file, it
    -- only needs to exist. Otherwise we would have to write something like
    -- `hPutStr handle modTxt`
    liftIO $ hClose handle
    liftIO $ mapM_ (logStr DEBUG) $ lines modTxt
    dynFlags <- getSessionDynFlags
    _ <-
      setSessionDynFlags $
        flip (foldl gopt_unset) setFlags $ -- Remove the HPC
          dynFlags
            { mainModIs = mkMainModule $ fsLit mname,
              mainFunIs = Just "main__",
              hpcDir = "./fake_targets",
              ghcMode = if repUseInterpreted then CompManager else OneShot,
              ghcLink = if repUseInterpreted then LinkInMemory else LinkBinary,
              hscTarget = if repUseInterpreted then HscInterpreted else HscAsm
              --optLevel = 2
            }
    now <- liftIO getCurrentTime
    let tid = TargetFile the_f Nothing
        target = Target tid True $ Just (strBuff, now)

    -- Adding and loading the target causes the compilation to kick
    -- off and compiles the file.
    addTarget target
    addLocalTargets [] (modBase cc)
    _ <- collectStats $ load LoadAllTargets
    let p '1' = Just True
        p '0' = Just False
        p _ = Nothing
        startCheck :: Int -> IO (Handle, ProcessHandle)
        startCheck which = do
          (_, Just hout, _, ph) <-
            createProcess
              (proc exeName [show which])
                { -- TODO: /dev/null should be NUL if we're on windows.
                  env = Just [("HPCTIXFILE", "/dev/null")],
                  -- We ignore the output
                  std_out = CreatePipe
                }
          return (hout, ph)
        waitOnCheck :: (Handle, ProcessHandle) -> IO TestSuiteResult
        waitOnCheck (hout, ph) = do
          ec <- timeout timeoutVal $ waitForProcess ph
          case ec of
            Nothing -> terminateProcess ph >> return (Right False)
            Just _ -> do
              res <- hGetLine hout
              let parsed = mapMaybe p res
              return $
                if length parsed == length res
                  then if and parsed then Right True else Left parsed
                  else Right False

    let inds = take (length fixes) [0 ..]
    if repUseInterpreted
      then do
        let m_name = mkModuleName mname
            checkArr arr = if and arr then Right True else Left arr
        setContext [IIDecl $ simpleImportDecl m_name]
        checks_expr <- compileExpr "checks__"
        let checks :: [IO [Bool]]
            checks = unsafeCoerce# checks_expr
            evf = if repParChecks then mapConcurrently else mapM
        liftIO $ collectStats $ evf (checkArr <$>) checks
      else
        liftIO $
          if repParChecks
            then do
              -- By starting all the processes and then waiting on them, we get more
              -- mode parallelism.
              procs <- collectStats $ mapM startCheck inds
              collectStats $ mapM waitOnCheck procs
            else collectStats $ mapM (startCheck >=> waitOnCheck) inds

describeProblem :: Configuration -> FilePath -> IO (Maybe ProblemDescription)
describeProblem conf@Conf {compileConfig = cc, repairConfig = repConf} fp = do
  logStr DEBUG "Describing problem..."
  (compConf, modul, problem) <- moduleToProb cc fp Nothing
  case problem of
    Just ExProb {} -> error "External targets not supported!"
    Nothing -> return Nothing
    Just progProblem@EProb {..} ->
      Just <$> do
        exprFitCands <- runGhc' cc $ getExprFitCands $ Right modul
        let probModule = Just modul
            initialFixes = Nothing
            desc' = ProbDesc {..}

        if repPrecomputeFixes repConf
          then do
            logStr DEBUG "Pre-computing fixes..."
            let inContext = noLoc . HsLet NoExtField e_ctxt
                addContext = snd . fromJust . flip fillHole (inContext hole) . unLoc
            nzh <- findEvaluatedHoles desc'
            fits <-
              collectStats $
                runGhc (Just libdir) $
                  getHoleFits compConf exprFitCands (map addContext nzh)
            let fix_cands :: [(EFix, EExpr)]
                fix_cands = map (first Map.fromList) (zip nzh fits >>= uncurry replacements)
                initialFixes' = Just $ map fst fix_cands
            logStr DEBUG "Initial fixes:"
            logOut DEBUG initialFixes'
            return $ desc' {initialFixes = initialFixes'}
          else return desc'
