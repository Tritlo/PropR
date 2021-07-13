{-# LANGUAGE FlexibleContexts #-}
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
import qualified CoreUtils
import Data.Bifunctor (Bifunctor (first))
import Data.Dynamic (fromDyn)
import Data.Either (lefts)
import Data.IORef (writeIORef)
import Data.List (intercalate, sortOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import qualified Data.Set as Set
import Data.Tree (flatten)
import Desugar (deSugarExpr)
import Endemic.Check
import Endemic.Eval
import Endemic.Traversals (fillHole, flattenExpr, sanctifyExpr)
import Endemic.Types
import Endemic.Util
import FV (fvVarSet)
import GHC
import GHC.Paths (libdir)
import GhcPlugins
import RnExpr (rnLExpr)
import TcExpr (tcInferSigma)
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

-- | Parse, rename and type check an expression
justTcExpr :: CompileConfig -> EExpr -> Ghc (Maybe ((LHsExpr GhcTc, Type), WantedConstraints))
justTcExpr cc parsed = do
  _ <- initGhcCtxt cc
  hsc_env <- getSession
  (_, res) <-
    liftIO $
      runTcInteractive hsc_env $ captureTopConstraints $ rnLExpr parsed >>= tcInferSigma . fst
  return res

-- | We get the type of the given expression by desugaring it and getting the type
-- of the resulting Core expression
getExprTy :: HscEnv -> LHsExpr GhcTc -> IO (Maybe Type)
getExprTy hsc_env expr = fmap CoreUtils.exprType . snd <$> deSugarExpr hsc_env expr

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
propCounterExample :: CompileConfig -> EProblem -> EProp -> IO (Maybe [RExpr])
propCounterExample cc ep prop = do
  let cc' = (cc {hole_lvl = 0, importStmts = checkImports ++ importStmts cc})
      bcc = buildCounterExampleCheck prop ep
  exec <- compileParsedCheck cc' bcc
  fromDyn exec (return Nothing)

-- | Takes an expression and generates HoleFitCandidates from every subexpression.
getExprFitCands ::
  -- | The general compiler setup
  CompileConfig ->
  -- | The expression to be holed
  EExpr ->
  IO [ExprFitCand]
getExprFitCands cc expr = runGhc (Just libdir) $ do
  -- setSessionDynFlags reads the package database.
  _ <- setSessionDynFlags =<< getSessionDynFlags
  -- If it type checks, we can use the expression
  mb_tcd_context <- justTcExpr cc expr
  let esAndNames =
        case mb_tcd_context of
          Just ((tcd_context, _), wc) ->
            -- We get all the expressions in the program here,
            -- so that we can  pass it along to our custom holeFitPlugin.
            let flat = flattenExpr tcd_context
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
                e_ids (L _ (HsVar _ v)) = Just $ unLoc v
                e_ids _ = Nothing
                -- We remove the ones already present and drop the first one
                -- (since it will be the program itself)
                flat' = filter nonTriv $ tail flat
             in map (\e -> (e, bagToList $ wc_simple wc, mapMaybe e_ids $ flattenExpr e)) flat'
          _ -> []
  hsc_env <- getSession
  -- After we've found the expressions and any ids contained within them, we
  -- need to find their types
  liftIO $
    mapM
      ( \(e, wc, rs) -> do
          ty <- getExprTy hsc_env e
          return $ case ty of
            Nothing -> EFC e emptyBag rs ty
            Just expr_ty -> EFC e (listToBag (relevantCts expr_ty wc)) rs ty
      )
      esAndNames
  where
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

-- | Returns the props that fail for the given program
failingProps :: CompileConfig -> EProblem -> IO [EProp]
failingProps _ EProb {e_props = []} = return []
-- Our method for checking which props fail is restricted to maximum 8 at a time,
-- so if we have more than that, we check the first 8 and then the rest, and
-- so on.
failingProps cc rp@EProb {e_props = ps} | length ps > 8 = do
  let (ps1, ps2) = splitAt 8 ps
  p1 <- failingProps cc rp {e_props = ps1}
  p2 <- failingProps cc rp {e_props = ps2}
  return (p1 ++ p2)
failingProps cc ep@EProb {..} = do
  let cc' = (cc {hole_lvl = 0, importStmts = checkImports ++ importStmts cc})
      check = buildSuccessCheck ep
  [compiled_check] <- compileParsedChecks cc' [check]
  ran <- runCheck compiled_check
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
              fp ps = failingProps cc ep {e_props = ps}
              ps1, ps2 :: [EProp]
              (ps1, ps2) = splitAt (length e_props `div` 2) e_props
          concat <$> mapM fp [ps1, ps2]

-- | Primary method of this module.
-- It takes a program & configuration,
-- a (translated) repair problem and returns a list of potential fixes.
repair :: CompileConfig -> EProblem -> IO [EFix]
repair cc prob = map fst . filter (\(_, r) -> r == Right True) <$> repairAttempt cc prob Nothing

-- | This method tries to repair a given Problem.
-- It first creates the program with holes in it and runs it against the properties.
-- From this, candidates are retrieved of touched holes and fixes are created and run.
-- Quite some information can be printed when the program is run in DEBUG.
-- As an important sidenote, places that are not in failing properties will not be altered.
repairAttempt ::
  -- | The GHC Configuration
  CompileConfig ->
  -- | The problem that is to fix, consisting of a program and a failing suite of properties
  EProblem ->
  -- | Manually passed Candidates, if "Nothing" collected from program runtime
  Maybe [ExprFitCand] ->
  IO [(EFix, Either [Bool] Bool)]
repairAttempt cc tp@EProb {..} efcs = collectStats $ do
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

  logOut TRACE prog_at_ty
  logOut TRACE holey_exprs

  -- We can use the failing_props and the counter_examples to filter
  -- out locations that we know won't matter.
  failing_props <- collectStats $ failingProps cc tp
  counter_examples <- collectStats $ mapM (propCounterExample cc tp) failing_props
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
        <$> traceTargets cc prog_at_ty ps_w_ce
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
  collectStats $ zipWith (\(fs, _) r -> (Map.fromList fs, r)) repls <$> checkFixes cc' tp (map snd repls)