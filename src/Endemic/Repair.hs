{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

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
import Data.Either (lefts, rights)
import Data.Foldable (find)
import Data.Function (on)
import Data.Functor (($>), (<&>))
import qualified Data.Functor
import Data.IORef (IORef, modifyIORef, modifyIORef', newIORef, readIORef, writeIORef)
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
import Exception
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
  res <- getHoleFits' plugRef (if holeLvl == 0 then 0 else holeDepth) exprs
  return $ map (map (map snd . snd) . snd) res
  where
    exprFits ::
      IORef HoleFitState ->
      [SrcSpan] ->
      LHsExpr GhcPs ->
      Ghc (Either [ValsAndRefs] HValue)
    exprFits plugRef l expr = do
      let act =
            liftIO (modifyIORef' plugRef resetHoleFitList)
              >> handleSourceError
                (getHoleFitsFromError plugRef l)
                (Right <$> compileParsedExpr expr)
      -- We need to try twice here, since defaulting can have a weird effect,
      -- and we might be missing out on some fits
      withDefaulting (Just []) act >>= \case
        -- It wasn't an error at all:
        Right a -> return $ Right a
        -- We got something, and no addtional errors:
        Left (Right x) -> return $ Left x
        -- We got a result, but we had additional errors:
        Left (Left (ovnd, oerrs)) -> do
          let ll = DEBUG
          liftIO $ logStr ll "While getting fits for:"
          liftIO $ logOut ll expr
          liftIO $ logStr ll "Got result with errors:"
          liftIO $ logOut ll ovnd
          liftIO $ logStr ll "The errors were:"
          liftIO $ mapM_ (logStr ll . show) oerrs
          -- We try the default defaults and some other defaults
          let different_defaults =
                Nothing : -- The default, defaults to integerTy and doubleTy
                if extendDefaults
                  then [extraDefaults] -- default to ints and floats
                  else []

              joinFits :: [ValsAndRefs] -> ValsAndRefs
              joinFits =
                (nubBy nf *** nubBy nf)
                  . foldr (\(vs, rfs) (vss, rfss) -> (vs ++ vss, rfs ++ rfss)) ([], [])
              nf hfa@HoleFit {} hfb@HoleFit {} = hfId hfa == hfId hfb
              nf (RawHoleFit a) (RawHoleFit b) = showSDocUnsafe a == showSDocUnsafe b
              nf _ _ = False
          -- Get all the results from all the defaults and then we join them.
          -- Note that the transpose is safe here, since the amount of holes
          -- is always the same.
          Left . map joinFits . transpose . rights . lefts
            <$> mapM (`withDefaulting` act) different_defaults

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
        mapM (\(l, e) -> mapLeft ((e,) . zip l) <$> exprFits plugRef l e) exprs
          >>= processFits . map (second (map (second fst))) . lefts
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags {refLevelHoleFits = Just 0}
      return res
    getHoleFits' plugRef n exprs = do
      fits <- lefts <$> mapM (\(l, e) -> mapLeft ((e,) . zip l) <$> exprFits plugRef l e) exprs
      procs <- processFits $ map (second $ map (second (uncurry (++)))) fits
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

extraDefaults :: Maybe [Type]
extraDefaults = Just [unitTy, listTy, intTy, floatTy] -- default to ints and floats
  where
    listTy = mkTyConApp listTyCon []

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
      e_prop_sigs = []
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
        let (num_args, bcc) = mk_bcc prop seed
        liftIO $ logOut DEBUG bcc
        exec <- dynCompileParsedExpr `reportOnError` bcc
        -- If the properties have additional choices, quickcheck reports those
        -- as well, so we must take care to remove these with the take.
        (take num_args . map addPar <$>) <$> liftIO (fromDyn exec (return Nothing))

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
    liftIO $ logStr TRACE "Finding evaluated holes..."
    -- We apply the fixes to all of the contexts, and all of the contexts
    -- contain the entire current program.
    -- TODO: Is this safe?
    let id_prog = eProgToEProgFixAtTy e_prog

    -- We can use the failing_props and the counter_examples to filter
    -- out locations that we know won't matter.
    liftIO $ logStr TRACE "Finding failing props..."
    failing_props <- collectStats $ failingProps' desc
    liftIO $ logStr TRACE $ "Found " ++ show (length failing_props) ++ " failing props"

    liftIO $ logStr TRACE "Finding counter examples..."
    counter_examples <- liftIO $ collectStats $ propCounterExamples desc failing_props
    liftIO $ logStr TRACE $ "Found " ++ show (length counter_examples) ++ " counter examples"

    let hasCE (p, Just ce) = Just (p, ce)
        hasCE _ = Nothing
        -- ps_w_ce = Properties with CounterExamples -> The Failing Properties
        ps_w_ce = mapMaybe hasCE $ zip failing_props counter_examples
    -- We compute the locations that are touched by the failing counter-examples
    -- liftIO $ logStr TRACE "Tracing program..."
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
    traces_that_worked :: [(EExpr, Map (EExpr, SrcSpan) Integer)] <-
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

    liftIO $ logStr TRACE $ "Got " ++ show (length traces_that_worked) ++ " traces that worked"
    liftIO $ mapM_ (logOut DEBUG . second (Map.filter (> 0))) traces_that_worked
    -- We then remove suggested holes that are unlikely to help (naively for now
    -- in the sense that we remove only holes which did not get evaluated at all,
    -- so they are definitely not going to matter).
    let fk :: (EExpr, Map (EExpr, SrcSpan) Integer) -> Set.Set (SrcSpan, LHsExpr GhcPs)
        fk (expr, invokes)
          | non_zero <- Map.keysSet (Map.filter (> 0) invokes),
            not (null non_zero) =
            Set.fromList $ mapMaybe toExprHole $ Set.toList non_zero
          where
            sfe = sanctifyExpr noExtField expr
            toExprHole (iv_expr, iv_loc) =
              -- We can get a Nothing here if e.g. the evaluated part is with
              -- an operator with precedence, e.g. a ++ b ++ c, because GHC
              -- doesn't do precedence until it renames. We *could* use the
              -- renamed `iv_expr` and rename the `expr` as well to get those
              -- locs... but then we'd have to switch the whole thing over to
              -- `LHsExpr GhcRn`, since those locs do not exsist in  the
              -- `LHsExpr GhcPs` yet, and then we have issues with compiling
              -- them to get the valid hole fits, since GHC has no built-in
              -- function `compileRnStmt`. So we'll make do for now.
              case find is_iv (zip sfe sfi) of
                Just (e@(e_loc, _), _) | isGoodSrcSpan e_loc -> Just e
                _ -> Nothing
              where
                sfi = sanctifyExpr noExtField iv_expr
                is_iv = (== iv_loc) . fst . snd
        fk _ = Set.empty
        non_zero_holes = Set.unions $ map fk traces_that_worked
    -- nubOrd deduplicates collections of sortable items. it's faster than other dedups.
    let nzh = Set.toList non_zero_holes
    liftIO $ logStr TRACE $ "Found " ++ show (length nzh) ++ " evaluated holes"
    return nzh
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
generateFixCandidates _ [] = return []
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
        ((_, one_ty, one_prog) : _) = e_prog
        wrapped_in_holes =
          if allowFunctionFits cc
            then map ((\l -> ([l], wrapExpr l (wrapInHole l) $ progAtTy one_prog one_ty)) . fst) nzh
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
    liftIO $ logStr TRACE "Fix candidates:"
    liftIO $ mapM_ (logOut DEBUG) fix_cands
    liftIO $ logStr TRACE "Those were all of them!"
    return fix_cands
generateFixCandidates _ _ = error "External problems not supported"

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
          log = modifyIORef msg_ref ((locstr ++ " " ++ showSDoc dflags msg) :)
          locstr = showSDoc dflags $ ppr loc
          reject = return ()
  void $ setSessionDynFlags dflags {log_action = logAction}
  res <- defaultErrorHandler (\err -> modifyIORef msg_ref (err :)) (FlushOut (return ())) act
  dflags' <- getSessionDynFlags
  void $ setSessionDynFlags dflags' {log_action = log_action dflags}
  msgs <- reverse <$> liftIO (readIORef msg_ref)
  return (res, msgs)

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
  desc@ProbDesc
    { compConf = cc@CompConf {..},
      progProblem = tp,
      addConf = AddConf {..},
      ..
    }
  -- Because checkFixes sets the dynFlags, we need to run it in a separate
  -- thread: hence the runGhc' here
  orig_fixes = runGhc' cc $ do
    liftIO $ logStr TRACE "Checking fixes..."
    orig_flags <- setCheckDynFlags
    (tempDir, exeName, mname, target_name, tid) <- initCompileChecks orig_flags orig_fixes
    liftIO $ logStr TRACE $ "Loading up to " ++ moduleNameString target_name
    (sf2, msgs) <- tryGHCCaptureOutput (load (LoadUpTo target_name))
    res <-
      if succeeded sf2
        then runCompiledFixes exeName mname orig_fixes
        else
          if not filterIncorrectFixes
            then do
              liftIO $
                logStr ERROR $
                  "Error while loading: " ++ moduleNameString target_name
                    ++ " see error message for more information"
              liftIO $ mapM_ (logStr ERROR) msgs
              liftIO exitFailure
            else do
              liftIO $ logStr TRACE "Error in fixes, trying to recover..."
              -- This might wreak havoc on the linker...
              removeTarget tid
              compiling <- zip orig_fixes <$> doesCompileBin orig_flags orig_fixes
              let comp_inds_n_e = filter (snd . snd) $ zip [0 :: Int ..] compiling
                  comp_inds = map fst comp_inds_n_e
                  compiling_fixes = map (fst . snd) comp_inds_n_e
              if (null compiling_fixes)
                then do
                  liftIO $
                    logStr TRACE $
                      "Error while loading: " ++ moduleNameString target_name
                        ++ " see error message for more information"
                  liftIO $ logStr TRACE $ "None of the " ++ show (length orig_fixes) ++ " fixes compiled, recovery failed"
                  liftIO $ logStr TRACE $ "Failed with the errors:"
                  liftIO $ mapM_ (logStr DEBUG) msgs
                  return (replicate (length orig_fixes) (Right False))
                else do
                  liftIO $ logStr TRACE $ show (length compiling_fixes) ++ " of " ++ show (length orig_fixes) ++ " were recovered..."
                  liftIO $ logStr TRACE $ "Reinitializing..."
                  setCheckDynFlags
                  (_, n_exe, n_mname, n_target, _) <- initCompileChecks orig_flags compiling_fixes
                  liftIO $ logStr TRACE $ "Loading up to " ++ moduleNameString n_target ++ " again..."
                  (sf2, msgs) <- tryGHCCaptureOutput (load $ LoadUpTo n_target)
                  if succeeded sf2
                    then liftIO $ logStr TRACE "Recovered!"
                    else do
                      liftIO $ logStr ERROR "Recovery completely failed!"
                      liftIO $ mapM_ (logStr ERROR) msgs
                      liftIO $ exitFailure
                  new_results <- runCompiledFixes n_exe n_mname compiling_fixes
                  let result_map = Map.fromList $ zip comp_inds new_results
                      getRes i = case result_map Map.!? i of
                        Just r -> r
                        _ -> Right False
                  return $ zipWith (\i _ -> getRes i) [0 :: Int ..] orig_fixes
    dflags <- getSessionDynFlags
    cleanupAfterLoads tempDir mname dflags
    return res
    where
      shouldInterpret = useInterpreted && assumeNoLoops
      timeoutVal = fromIntegral timeout

      doesCompileBin :: DynFlags -> [EProgFix] -> Ghc [Bool]
      -- Wow, so applicative
      doesCompileBin _ [] = return []
      doesCompileBin dflags [fix] = (: []) <$> doesCompile dflags [fix]
      doesCompileBin dflags fixes = (++) <$> checkHalf fh <*> checkHalf sh
        where
          (fh, sh) = splitAt (length fixes `div` 2) fixes
          checkHalf [] = return []
          checkHalf h = do
            c <- doesCompile dflags h
            if c
              then return (replicate (length h) True)
              else doesCompileBin dflags h

      doesCompile :: DynFlags -> [EProgFix] -> Ghc Bool
      doesCompile dflags fixes = do
        (tempDir, _, mname, target_name, tid) <- initCompileChecks dflags fixes
        liftIO $ logStr TRACE $ "Loading up to " ++ moduleNameString target_name
        (sf2, msgs) <- tryGHCCaptureOutput (load (LoadUpTo target_name))
        liftIO $ mapM_ (logStr DEBUG) msgs
        -- removeTarget might not be enough for the linker, we'll probably
        -- get duplicate symbol complaints....
        removeTarget tid
        cleanupAfterLoads tempDir mname dflags
        return (succeeded sf2)

      setCheckDynFlags :: Ghc DynFlags
      setCheckDynFlags = do
        dynFlags <- getSessionDynFlags
        let dflags' =
              -- turn-off all warnings
              flip (foldl wopt_unset) [toEnum 0 ..] $
                flip (foldl gopt_unset) setFlags $ -- Remove the HPC
                  dynFlags
                    { -- ghcMode = OneShot is the default, if we want it to compile
                      -- the files. But we've already compiled them at this point,
                      -- so we want it to use the CompManager to pick up the
                      -- dependencies.
                      ghcMode = CompManager,
                      -- TODO: Should this be LinkDynLib for MacOS?
                      ghcLink = if shouldInterpret then LinkInMemory else LinkBinary,
                      hscTarget = if shouldInterpret then HscInterpreted else HscAsm,
                      optLevel = if shouldInterpret then 0 else 2
                    }
        void $ setSessionDynFlags $ dflags'
        return dflags'

      initCompileChecks :: DynFlags -> [EProgFix] -> Ghc (FilePath, FilePath, [Char], ModuleName, TargetId)
      initCompileChecks dynFlags fixes = do
        seed <- liftIO newSeed
        let checkHash = flip showHex "" $ abs $ hashString $ showSDocUnsafe $ ppr (tp, fixes, seed)
            tempDir = tempDirBase </> "checks" </> checkHash
            the_f = tempDir </> ("FakeCheckTarget" ++ checkHash) <.> "hs"
            -- We generate the name of the module from the temporary file
            mname = filter isAlphaNum $ dropExtension $ takeFileName the_f
            modTxt = exprToCheckModule cc seed mname tp fixes
            exeName = dropExtension the_f
        liftIO $ createDirectoryIfMissing True tempDir
        -- Note: we do not need to dump the text of the module into the file, it
        -- only needs to exist. Otherwise we would have to write something like
        -- `hPutStr handle modTxt`
        liftIO $ writeFile the_f modTxt
        liftIO $ mapM_ (logStr DEBUG) $ lines modTxt
        now <- liftIO getCurrentTime
        let tid = TargetFile the_f Nothing
            target = Target tid True Nothing

        -- Adding and loading the target causes the compilation to kick
        -- off and compiles the file.
        target_name <- addTargetGetModName target
        _ <-
          setSessionDynFlags $
            dynFlags
              { mainModIs = mkModule mainUnitId target_name,
                mainFunIs = Just "main__",
                importPaths = importPaths dynFlags ++ modBase,
                hpcDir = tempDir
              }
        return (tempDir, exeName, mname, target_name, tid)

      runCompiledFixes :: FilePath -> String -> [EProgFix] -> Ghc [TestSuiteResult]
      runCompiledFixes exeName mname working_fixes = do
        liftIO $ logStr TRACE "Running compiled fixes..."
        mg <- depanal [] True
        liftIO $ logStr TRACE "New graph:"
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
                logStr TRACE $ "Checking " ++ show which ++ ":"
                logOut DEBUG (working_fixes !! which)
                logStr TRACE $ "Starting " ++ exeName
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
        let inds = take (length working_fixes) [0 ..]
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
        let interpretLoop checks = collectStats $ do
              logStr TRACE "Running checks..."
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
              logStr TRACE "Done checking!"
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
              collectStats $
                if parChecks
                  then do
                    -- By starting all the processes and then waiting on them, we get more
                    -- mode parallelism.
                    liftIO $ logStr TRACE "Running checks..."
                    procs <- collectStats $ mapM startCheck inds
                    collectStats $ mapM waitOnCheck procs
                  else collectStats $ mapM (startCheck >=> waitOnCheck) inds
        concat
          <$> if shouldInterpret
            then do
              liftIO $ logStr TRACE $ "Adding " ++ mname
              setContext [IIDecl $ simpleImportDecl m_name]
              liftIO $ logStr TRACE "Compiling checks__"
              checks_expr <- compileExpr "checks__"
              let checks :: [IO [Bool]]
                  checks = unsafeCoerce# checks_expr
              liftIO $ mapM interpretLoop $ chunkList batchSize checks
            else liftIO $ mapM nonInterpretLoop $ chunkList batchSize inds

describeProblem :: Configuration -> FilePath -> IO (Maybe ProblemDescription)
describeProblem conf@Conf {compileConfig = ogcc} fp = collectStats $ do
  logStr TRACE "Describing problem..."
  (compConf@CompConf {..}, modul, problem) <- moduleToProb ogcc fp Nothing
  case problem of
    Just ExProb {} -> error "External targets not supported!"
    Nothing -> return Nothing
    Just progProblem@EProb {..} ->
      Just <$> do
        exprFitCands <- runGhc' compConf $ getExprFitCands $ Right modul
        let probModule = Just modul
            initialFixes = Nothing
            addConf = def {assumeNoLoops = True}
            descBase = ProbDesc {..}
        if precomputeFixes
          then do
            logStr TRACE "Pre-computing fixes..."
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
            logStr TRACE $ "Found  " ++ show (length initialFixes) ++ " initial fixes:"
            mapM_ (logOut DEBUG) initialFixes'
            return $ descBase {initialFixes = Just initialFixes'}
          else return descBase