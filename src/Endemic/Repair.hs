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
--
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
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (when, (>=>))
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isAlphaNum)
import Data.Dynamic (fromDyn)
import Data.Either (lefts)
import Data.IORef (writeIORef)
import Data.List (intercalate, sortOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime)
import Data.Tree (flatten)
import Desugar (deSugarExpr)
import Endemic.Check
import Endemic.Configuration
import Endemic.Eval
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
import System.FilePath (dropExtension, takeFileName)
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
-- replacement-elements for the holes.
getHoleFits ::
  CompileConfig ->
  -- |  A given Compiler Config
  [ExprFitCand] ->
  -- | A list of existing and reachable candidate-expression
  [LHsExpr GhcPs] ->
  -- | The existing Expressions including holes
  IO [[[HoleFit]]]
getHoleFits cc local_exprs exprs =
  runGhc (Just libdir) $ do
    plugRef <- initGhcCtxt' True cc local_exprs
    -- Then we can actually run the program!
    setNoDefaulting
    let exprFits expr =
          liftIO (writeIORef plugRef [])
            >> handleSourceError
              (getHoleFitsFromError plugRef)
              (Right <$> compileParsedExpr expr)
    map (map fst) . lefts <$> mapM exprFits exprs

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
  e_prog <- parseExprNoInit r_prog
  ~(L _ (ExprWithTySig _ _ e_ty)) <- parseExprNoInit ("undefined :: " ++ r_ty)
  let clt = "let {" ++ (intercalate "; " . concatMap lines $ r_ctxt) ++ "} in undefined"
  ~(L _ (HsLet _ e_ctxt _)) <- parseExprNoInit clt
  let plt = "let {" ++ (intercalate "; " . concatMap lines $ r_props) ++ "} in undefined"
  ~(L _ (HsLet _ (L _ (HsValBinds _ (ValBinds _ lbs _))) _)) <- parseExprNoInit plt
  let e_props = bagToList lbs
      e_target = mkVarUnqual $ fsLit r_target
  return (EProb {..})

detranslate :: HasCallStack => EProblem -> RProblem
detranslate EProb {..} =
  let r_prog = showUnsafe e_prog
      r_ty = showUnsafe e_ty
      r_props = map showUnsafe e_props
      r_target = showUnsafe e_target
      (L _ (HsValBinds _ (ValBinds _ bs sigs))) = e_ctxt
      r_ctxt = map showUnsafe sigs ++ map showUnsafe (bagToList bs)
   in RProb {..}

-- | Get a list of strings which represent shrunk arguments to the property that
-- makes it fail.
propCounterExample :: RepairConfig -> CompileConfig -> EProblem -> EProp -> IO (Maybe [RExpr])
propCounterExample _ _ _ prop | isTastyProp prop = return $ Just []
  where
    -- TODO: If we had the type here as well, we could do better.
    isTastyProp :: LHsBind GhcPs -> Bool
    isTastyProp (L _ FunBind {fun_id = fid}) =
      "prop" /= take 4 (occNameString $ rdrNameOcc $ unLoc fid)
    isTastyProp _ = True
propCounterExample rc cc ep prop = do
  let cc' = (cc {hole_lvl = 0, importStmts = checkImports ++ importStmts cc})
      bcc = buildCounterExampleCheck rc (qcSeed cc) prop ep
  exec <- compileParsedCheck cc' bcc
  fromDyn exec (return Nothing)

-- | Returns the props that fail for the given program
failingProps :: RepairConfig -> CompileConfig -> EProblem -> IO [EProp]
failingProps _ _ EProb {e_props = []} = return []
-- Our method for checking which props fail is restricted to maximum 8 at a time,
-- so if we have more than that, we check the first 8 and then the rest, and
-- so on.
failingProps rc cc rp@EProb {e_props = ps} | length ps > 8 = do
  let (ps1, ps2) = splitAt 8 ps
  p1 <- failingProps rc cc rp {e_props = ps1}
  p2 <- failingProps rc cc rp {e_props = ps2}
  return (p1 ++ p2)
failingProps rc cc ep@EProb {..} = do
  let cc' = (cc {hole_lvl = 0, importStmts = checkImports ++ importStmts cc})
      check = buildSuccessCheck rc (qcSeed cc) ep
  [compiled_check] <- compileParsedChecks cc' [check]
  ran <- runCheck rc compiled_check
  case ran of
    -- Some of the props are failing:
    Left p -> return $ map fst $ filter (not . snd) $ zip e_props p
    -- None of the props are failing:
    Right True -> return []
    -- One of the props is causing an error/infinite loop, so we need
    -- to check each individually
    Right False ->
      case e_props of
        -- If there's only one failing prop left, that's the one causing
        -- the loop
        [prop] -> return [prop]
        -- Otherwise, we split the props into two sets, and check each
        -- split individually.
        _ -> do
          let fp :: [EProp] -> IO [EProp]
              fp ps = failingProps rc cc ep {e_props = ps}
              ps1, ps2 :: [EProp]
              (ps1, ps2) = splitAt (length e_props `div` 2) e_props
          concat <$> mapM fp [ps1, ps2]

-- | Primary method of this module.
-- It takes a program & configuration,
-- a (translated) repair problem and returns a list of potential fixes.
repair :: CompileConfig -> RepairConfig -> EProblem -> IO [EFix]
repair cc rc prob = map fst . filter (\(_, r) -> r == Right True) <$> repairAttempt cc rc prob Nothing

-- | This method tries to repair a given Problem.
-- It first creates the program with holes in it and runs it against the properties.
-- From this, candidates are retrieved of touched holes and fixes are created and run.
-- Quite some information can be printed when the program is run in DEBUG.
-- As an important sidenote, places that are not in failing properties will not be altered.
repairAttempt ::
  CompileConfig ->
  RepairConfig ->
  -- | The problem that is to fix, consisting of a program and a failing suite of properties
  EProblem ->
  -- | Manually passed Candidates, if "Nothing" collected from program runtime
  Maybe [ExprFitCand] ->
  IO [(EFix, Either [Bool] Bool)]
repairAttempt cc rc tp@EProb {..} efcs = collectStats $ do
  let prog_at_ty = progAtTy e_prog e_ty
      holey_exprs = sanctifyExpr prog_at_ty
      -- We add the context by replacing a hole in a let.
      inContext = noLoc . HsLet NoExtField e_ctxt
      holeyContext = inContext hole

  -- We find expressions that can be used as candidates in the program. Since
  -- these remain the same between attempts for the same program, we allow it
  -- to be precomputed.
  expr_cands <- case efcs of
    Just cands -> return cands
    _ -> collectStats $ getExprFitCands cc $ inContext $ noLoc undefVar

  trace_correl <- buildTraceCorrel cc prog_at_ty

  logOut DEBUG prog_at_ty
  logOut DEBUG holey_exprs

  -- We can use the failing_props and the counter_examples to filter
  -- out locations that we know won't matter.
  failing_props <- collectStats $ failingProps rc cc tp

  -- It only makes sense to generate counter-examples for quickcheck properties.
  counter_examples <- collectStats $ mapM (propCounterExample rc cc tp) failing_props

  let hasCE (p, Just ce) = Just (p, ce)
      hasCE _ = Nothing
      -- We find the ones with counter-examples and pick the shortest one
      ps_w_ce =
        sortOn (length . snd) $ mapMaybe hasCE $ zip failing_props counter_examples
  let only_max (src, r) = (mkInteractive src, maximum $ map snd r)
      toInvokes res = Map.fromList $ map only_max $ flatten res
  invokes <-
    collectStats $
      Map.toList
        . Map.unionsWith (+)
        . map toInvokes
        . catMaybes
        <$> traceTargets rc cc prog_at_ty ps_w_ce
  -- We then remove suggested holes that are unlikely to help (naively for now
  -- in the sense that we remove only holes which did not get evaluated at all,
  -- so they are definitely not going to matter).
  let non_zero = filter ((> 0) . snd) invokes
      non_zero_src = Set.fromList $ mapMaybe ((trace_correl Map.!?) . fst) non_zero
      non_zero_holes :: [(SrcSpan, LHsExpr GhcPs)]
      non_zero_holes = filter ((`Set.member` non_zero_src) . fst) holey_exprs
      addContext = snd . fromJust . flip fillHole holeyContext . unLoc
      nzh = map snd non_zero_holes

  fits <- collectStats $ zip nzh <$> getHoleFits cc expr_cands (map addContext nzh)
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

  processed_fits <-
    runGhc (Just libdir) $
      initGhcCtxt cc >> mapM (\(e, fs) -> (e,) <$> mapM (mapM processFit) fs) fits

  let repls = processed_fits >>= uncurry replacements

  logStr DEBUG "Fix candidates:"
  mapM_ (logOut DEBUG) repls
  logStr DEBUG "Those were all of them!"
  let cc' = (cc {hole_lvl = 0, importStmts = checkImports ++ importStmts cc})
  collectStats $ zipWith (\(fs, _) r -> (Map.fromList fs, r)) repls <$> checkFixes cc rc tp (map snd repls)

-- TODO: DOCUMENT
-- Returns an empty list when the EExpr list is empty
checkFixes ::
  CompileConfig ->
  RepairConfig ->
  EProblem ->
  [EExpr] ->
  IO [Either [Bool] Bool]
checkFixes cc rc tp fixes = do
  let RepConf {..} = rc
      tempDir = "./fake_targets"
  createDirectoryIfMissing False tempDir
  (the_f, handle) <- openTempFile tempDir "FakeTargetCheck.hs"
  -- We generate the name of the module from the temporary file
  let mname = filter isAlphaNum $ dropExtension $ takeFileName the_f
      modTxt = exprToCheckModule rc cc mname tp fixes
      strBuff = stringToStringBuffer modTxt
      exeName = dropExtension the_f
      timeoutVal = fromIntegral repTimeout
  -- mixFilePath = tempDir

  logStr DEBUG modTxt
  -- Note: we do not need to dump the text of the module into the file, it
  -- only needs to exist. Otherwise we would have to write something like
  -- `hPutStr handle modTxt`
  hClose handle
  liftIO $ mapM_ (logStr DEBUG) $ lines modTxt
  runGhc (Just libdir) $ do
    _ <- initGhcCtxt cc
    -- We set the module as the main module, which makes GHC generate
    -- the executable.
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
    _ <- collectStats $ load LoadAllTargets
    let p '1' = Just True
        p '0' = Just False
        p _ = Nothing
        startCheck :: Int -> IO (Handle, ProcessHandle)
        startCheck which = do
          let tixFilePath = exeName ++ "_" ++ show which ++ ".tix"
          (_, Just hout, _, ph) <-
            createProcess
              (proc exeName [show which])
                { env = Just [("HPCTIXFILE", tixFilePath)],
                  -- We ignore the output
                  std_out = CreatePipe
                }
          return (hout, ph)
        waitOnCheck :: (Handle, ProcessHandle) -> IO (Either [Bool] Bool)
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
