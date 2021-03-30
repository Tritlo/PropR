{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Synth.Eval where

-- GHC API

import Bag
import Constraint (Ct (..), holeOcc)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Bifunctor
import Data.Bits (complement)
import Data.Char (isAlphaNum)
import Data.Data
import Data.Dynamic
import Data.Either
import Data.Function (on)
import Data.IORef
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Time.Clock
import Data.Tree
import DriverPhases (Phase (..))
import DynFlags
import ErrUtils (errDocImportant, errDocSupplementary, errMsgDoc)
import GHC
import GHC.Paths (libdir)
import GHC.Prim (unsafeCoerce#)
import GHC.Stack (HasCallStack)
import GhcPlugins
  ( HscEnv (hsc_IC),
    HscSource (..),
    InteractiveContext (ic_default),
    OccName (..),
    PluginWithArgs (..),
    StaticPlugin (..),
    concatFS,
    fsLit,
    getRdrName,
    interactiveSrcLoc,
    mkOccNameFS,
    mkVarUnqual,
    occName,
    occNameString,
    substTyWith,
  )
import HscTypes (SourceError, srcErrorMessages)
import Outputable hiding (char)
import PrelNames (mkMainModule, toDynName)
import StringBuffer
import Synth.Check
import Synth.Flatten
import Synth.Plugin
import Synth.Types
import Synth.Util
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Posix.Process
import System.Posix.Signals
import System.Process
import System.Timeout
import TcHoleErrors (HoleFit (..), TypedHole (..))
import Trace.Hpc.Mix
import Trace.Hpc.Tix
import Trace.Hpc.Util
import TysWiredIn (unitTy)

-- Configuration and GHC setup

holeFlags =
  [ Opt_ShowHoleConstraints,
    Opt_ShowProvOfHoleFits,
    Opt_ShowTypeAppVarsOfHoleFits,
    Opt_ShowTypeAppOfHoleFits,
    Opt_ShowTypeOfHoleFits
  ]

setFlags = [Opt_Hpc]

config :: Int -> DynFlags -> DynFlags
config lvl sflags =
  flip (foldl gopt_set) setFlags $
    (foldl gopt_unset sflags (Opt_OmitYields : holeFlags))
      { maxValidHoleFits = Nothing,
        maxRefHoleFits = Nothing,
        refLevelHoleFits = Just lvl
      }

-- UTIL

output :: Outputable p => [p] -> Ghc ()
output p = do
  flags <- getSessionDynFlags
  dbg <- liftIO hasDebug
  when dbg $
    mapM_ (liftIO . print . showSDoc flags . ppr) p

----

data CompileConfig = CompConf
  { importStmts :: [String],
    packages :: [String],
    hole_lvl :: Int
  }
  deriving (Show, Eq, Ord)

toPkg :: String -> PackageFlag
toPkg str = ExposePackage ("-package " ++ str) (PackageArg str) (ModRenaming True [])

-- InitGhcCtxt initializes the context and the hole fit plugin with no
-- expression fit candidates
initGhcCtxt :: CompileConfig -> Ghc (IORef [(TypedHole, [HoleFit])])
initGhcCtxt cc = initGhcCtxt' cc []

-- initGhcCtxt' intializes the hole fit plugin we use to extract fits and inject
-- expression fits, as well as adding any additional imports.
initGhcCtxt' :: CompileConfig -> [ExprFitCand] -> Ghc (IORef [(TypedHole, [HoleFit])])
initGhcCtxt' CompConf {..} local_exprs = do
  flags <- config hole_lvl <$> getSessionDynFlags
  --`dopt_set` Opt_D_dump_json
  -- First we have to add "base" to scope
  plugRef <- liftIO $ newIORef []
  let flags' =
        flags
          { packageFlags =
              packageFlags flags
                ++ map toPkg packages,
            staticPlugins = sPlug : staticPlugins flags
          }
      sPlug =
        StaticPlugin $
          PluginWithArgs
            { paArguments = [],
              paPlugin = synthPlug local_exprs plugRef
            }
  toLink <- setSessionDynFlags flags'
  -- "If you are not doing linking or doing static linking, you can ignore the list of packages returned."
  --(hsc_dynLinker <$> getSession) >>= liftIO . (flip extendLoadedPkgs toLink)
  -- Then we import the prelude and add it to the context
  imports <- mapM (fmap IIDecl . parseImportDecl) importStmts
  getContext >>= setContext . (imports ++)
  return plugRef

justParseExpr :: CompileConfig -> RExpr -> Ghc (LHsExpr GhcPs)
justParseExpr cc str = do
  plugRef <- initGhcCtxt cc
  parseExprNoInit str

parseExprNoInit :: RExpr -> Ghc (LHsExpr GhcPs)
parseExprNoInit str =
  handleSourceError
    (\err -> printException err >> error "parse failed")
    (parseExpr str)

runJustParseExpr :: CompileConfig -> RExpr -> IO (LHsExpr GhcPs)
runJustParseExpr cc str = runGhc (Just libdir) $ justParseExpr cc str

type ValsAndRefs = ([HoleFit], [HoleFit])

type CompileRes = Either [ValsAndRefs] Dynamic

-- By integrating with a hole fit plugin, we can extract the fits (with all
-- the types and everything directly, instead of having to parse the error
-- message)
getHoleFitsFromError ::
  IORef [(TypedHole, [HoleFit])] ->
  SourceError ->
  Ghc (Either [ValsAndRefs] b)
getHoleFitsFromError plugRef err = do
  flags <- getSessionDynFlags
  dbg <- liftIO hasDebug
  when dbg $ printException err
  res <- liftIO $ readIORef plugRef
  when (null res) (printException err)
  let gs = groupBy (sameHole `on` fst) res
      allFitsOfHole ((th, f) : rest) = (th, concat $ f : map snd rest)
      valsAndRefs = map ((partition part . snd) . allFitsOfHole) gs
  return $ Left valsAndRefs
  where
    part (RawHoleFit _) = True
    part HoleFit {..} = hfRefLvl <= 0
    sameHole :: TypedHole -> TypedHole -> Bool
    sameHole
      TyH {tyHCt = Just CHoleCan {cc_hole = h1}}
      TyH {tyHCt = Just CHoleCan {cc_hole = h2}} =
        holeOcc h1 == holeOcc h2
    sameHole _ _ = False

monomorphiseType :: CompileConfig -> RType -> IO (Maybe RType)
monomorphiseType cc ty =
  runGhc (Just libdir) $
    do
      _ <- initGhcCtxt cc
      flags <- getSessionDynFlags
      let pp = showSDoc flags . ppr
      handleSourceError
        (const $ return Nothing)
        (Just . pp . mono <$> exprType TM_Default ("undefined :: " ++ ty))
  where
    mono ty = substTyWith tvs (replicate (length tvs) unitTy) base_ty
      where
        (tvs, base_ty) = splitForAllTys ty

evalOrHoleFits :: CompileConfig -> RExpr -> Ghc CompileRes
evalOrHoleFits cc str = do
  plugRef <- initGhcCtxt cc
  -- Then we can actually run the program!
  handleSourceError
    (getHoleFitsFromError plugRef)
    (Right <$> dynCompileExpr str)

moduleToProb ::
  CompileConfig ->
  FilePath ->
  Maybe String ->
  IO (CompileConfig, ParsedModule, [RProblem])
moduleToProb cc@CompConf {..} mod_path mb_target = do
  let target = Target (TargetFile mod_path Nothing) True Nothing
  runGhc (Just libdir) $ do
    _ <- initGhcCtxt cc
    addTarget target
    _ <- load LoadAllTargets
    let mname = mkModuleName $ dropExtension $ takeFileName mod_path
    mod@ParsedModule {..} <- getModSummary mname >>= parseModule
    let (L _ HsModule {..}) = pm_parsed_source
        cc' = cc {importStmts = importStmts ++ imps'}
          where
            imps' = map showUnsafe hsmodImports
        ctxt = map showUnsafe hsmodDecls
        props = filter isProp hsmodDecls
          where
            isProp (L _ (ValD _ FunBind {..})) =
              ((==) "prop" . take 4 . occNameString . occName . unLoc) fun_id
            isProp _ = False

        fix_targets = Set.toList $ fun_ids `Set.intersection` prop_vars
          where
            funId (L _ (ValD _ FunBind {..})) = Just (unLoc fun_id)
            funId _ = Nothing
            fun_ids = Set.fromList $ mapMaybe funId hsmodDecls
            mbVar (L _ (HsVar _ v)) = Just (unLoc v)
            mbVar _ = Nothing
            prop_vars =
              Set.fromList $
                mapMaybe mbVar $
                  concatMap (flattenBind . (\(ValD _ b) -> noLoc b) . unLoc) props

        getTarget :: RdrName -> Maybe RProblem
        getTarget t_name =
          case prog_sig of
            Just s ->
              Just $
                RProb
                  { r_target = fix_target,
                    r_prog = showUnsafe $ wp_expr s,
                    r_ctxt = ctxt,
                    r_ty = prog_ty s,
                    r_props = wrapped_props
                  }
            _ -> Nothing
          where
            fix_target = showUnsafe t_name
            isTDef (L _ (SigD _ (TypeSig _ ids _))) = t_name `elem` map unLoc ids
            isTDef (L _ (ValD _ FunBind {..})) = t_name == unLoc fun_id
            isTDef _ = False
            -- We get the type of the program
            getTType (L _ (SigD _ ts@(TypeSig _ ids sig)))
              | t_name `elem` map unLoc ids =
                Just ts
            getTType _ = Nothing
            -- takes prop :: t ==> prop' :: target_type -> t since our
            -- previous assumptions relied on the properties to take in the
            -- function being fixed  as the first argument.
            wrapProp pdef = unwords (p' : fix_target : rest)
              where
                (p : rest) = words $ showUnsafe pdef
                p' = "prop'" ++ drop 4 p
            wrapped_props = map wrapProp props
            prog_binds :: LHsBindsLR GhcPs GhcPs
            prog_binds = listToBag $ mapMaybe f $ filter isTDef hsmodDecls
              where
                f (L _ (ValD _ b)) = Just $ noLoc b
                f _ = Nothing
            prog_sig :: Maybe (Sig GhcPs)
            prog_sig = case mapMaybe getTType hsmodDecls of
              (pt : _) -> Just pt
              _ -> Nothing
            prog_ty :: Sig GhcPs -> String
            prog_ty prog_sig = showUnsafe $ hsib_body $ hswc_body sig
              where
                (TypeSig _ _ sig) = prog_sig
            wp_expr :: Sig GhcPs -> LHsExpr GhcPs
            wp_expr prog_sig = noLoc $ HsLet noExtField (noLoc lbs) (noLoc le)
              where
                le = HsVar noExtField (noLoc t_name)
                lbs =
                  HsValBinds noExtField $
                    ValBinds noExtField prog_binds [noLoc prog_sig]
        probs = case mb_target of
          Just t ->
            case getTarget (mkVarUnqual $ fsLit t) of
              Just r -> [r]
              _ -> error $ "Could not find type of the target `" ++ t ++ "`!"
          Nothing -> mapMaybe getTarget fix_targets
    return (cc', mod, probs)

-- Create a fake base loc for a trace.
fakeBaseLoc :: CompileConfig -> EExpr -> IO SrcSpan
fakeBaseLoc = fmap (getLoc <$>) . buildTraceCorrelExpr

-- When we do the trace, we use a "fake_target" function. This build the
-- corresponding expression,
buildTraceCorrelExpr :: CompileConfig -> EExpr -> IO (LHsExpr GhcPs)
buildTraceCorrelExpr cc expr = do
  let correl = baseFun (mkVarUnqual $ fsLit "fake_target") expr
      correl_ctxt = noLoc $ HsValBinds NoExtField (ValBinds NoExtField (unitBag correl) [])
      correl_expr = (noLoc $ HsLet NoExtField correl_ctxt hole) :: LHsExpr GhcPs
  pcorrel <- runJustParseExpr cc $ showUnsafe correl_expr
  let (L _ (HsLet _ (L _ (HsValBinds _ (ValBinds _ bg _))) _)) = pcorrel
      [L _ FunBind {fun_matches = MG {mg_alts = (L _ alts)}}] = bagToList bg
      [L _ Match {m_grhss = GRHSs {grhssGRHSs = [L _ (GRHS _ _ bod)]}}] = alts
  return bod

-- We build a Map from the traced expression and to the  original so we can
-- correlate the trace information with the expression we're checking.
buildTraceCorrel :: CompileConfig -> EExpr -> IO (Map.Map SrcSpan SrcSpan)
buildTraceCorrel cc expr =
  Map.fromList
    . filter (\(b, e) -> isGoodSrcSpan b && isGoodSrcSpan e)
    . flip (zipWith (\b e -> (getLoc b, getLoc e))) (flattenExpr expr)
    . flattenExpr
    <$> buildTraceCorrelExpr cc expr

-- Run HPC to get the trace information.
traceTarget ::
  CompileConfig ->
  EExpr ->
  EProp ->
  [RExpr] ->
  IO (Maybe (Tree (SrcSpan, [(BoxLabel, Integer)])))
traceTarget
  cc
  expr@(L (RealSrcSpan realSpan) _)
  failing_prop
  failing_args = do
    let tempDir = "./fake_targets"
    createDirectoryIfMissing False tempDir
    (tf, handle) <- openTempFile tempDir "FakeTarget.hs"
    -- We generate the name of the module from the temporary file
    let mname = filter isAlphaNum $ dropExtension $ takeFileName tf
        correl = baseFun (mkVarUnqual $ fsLit "fake_target") expr
        modTxt = exprToModule cc mname correl (showUnsafe failing_prop) failing_args
        strBuff = stringToStringBuffer modTxt
        m_name = mkModuleName mname
        mod = IIModule m_name
        exeName = dropExtension tf
        tixFilePath = exeName ++ ".tix"
        mixFilePath = tempDir

    prDebug modTxt
    -- Note: we do not need to dump the text of the module into the file, it
    -- only needs to exist. Otherwise we would have to write something like
    -- `hPutStr handle modTxt`
    hClose handle
    liftIO $ mapM prDebug $ lines modTxt
    runGhc (Just libdir) $
      do
        plugRef <- initGhcCtxt cc
        -- We set the module as the main module, which makes GHC generate
        -- the executable.
        dynFlags <- getSessionDynFlags
        setSessionDynFlags $
          dynFlags
            { mainModIs = mkMainModule $ fsLit mname,
              hpcDir = "./fake_targets"
            }
        now <- liftIO getCurrentTime
        let tid = TargetFile tf Nothing
            target = Target tid True $ Just (strBuff, now)

        -- Adding and loading the target causes the compilation to kick
        -- off and compiles the file.
        addTarget target
        _ <- load LoadAllTargets
        -- We should for here in case it doesn't terminate, and modify
        -- the run function so that it use the trace reflect functionality
        -- to timeout and dump the tix file if possible.
        res <- liftIO $
          do
            (_, _, _, ph) <-
              createProcess
                (proc exeName [])
                  { env = Just [("HPCTIXFILE", tixFilePath)],
                    -- We ignore the output
                    std_out = CreatePipe
                  }
            ec <- timeout timeoutVal $ waitForProcess ph

            let -- If it doesn't respond to signals, we can't do anything
                -- other than terminate
                loop ec 0 = terminateProcess ph
                loop ec n = when (isNothing ec) $ do
                  -- If it's taking too long, it's probably stuck in a loop.
                  -- By sending the right signal though, it will dump the tix
                  -- file before dying.
                  pid <- getPid ph
                  case pid of
                    Just pid ->
                      do
                        signalProcess keyboardSignal pid
                        ec2 <- timeout timeoutVal $ waitForProcess ph
                        loop ec2 (n -1)
                    _ ->
                      -- It finished in the brief time between calls, so we're good.
                      return ()
            -- We give it 3 tries
            loop ec 3

            tix <- readTix tixFilePath
            let rm m = (m,) <$> readMix [mixFilePath] (Right m)
            case tix of
              Just (Tix mods) -> do
                -- We throw away any extra functions in the file, such as
                -- the properties and the main function, and only look at
                -- the ticks for our expression
                [n@Node {rootLabel = (root, _)}] <- filter isTarget . concatMap toDom <$> mapM rm mods
                return $ Just (fmap (Data.Bifunctor.first (toFakeSpan tf root)) n)
              _ -> return Nothing
        removeTarget tid
        liftIO $ removeDirectoryRecursive tempDir
        return res
    where
      toDom :: (TixModule, Mix) -> [MixEntryDom [(BoxLabel, Integer)]]
      toDom (TixModule _ _ _ ts, Mix _ _ _ _ es) =
        createMixEntryDom $ zipWith (\t (pos, bl) -> (pos, (bl, t))) ts es
      isTarget Node {rootLabel = (root, [(TopLevelBox ["fake_target"], _)])} =
        True
      isTarget _ = False
      -- We convert the HpcPos to the equivalent span we would get if we'd
      -- parsed and compiled the expression directly.
      toFakeSpan :: FilePath -> HpcPos -> HpcPos -> SrcSpan
      toFakeSpan tf root sp = mkSrcSpan start end
        where
          fname = fsLit $ takeFileName tf
          (rsl, rsc, rel, rec) = fromHpcPos root
          eloff = rel - srcSpanEndLine realSpan
          ecoff = rec - srcSpanEndCol realSpan
          (sl, sc, el, ec) = fromHpcPos sp
          -- We add two spaces before every line in the source.
          start = mkSrcLoc fname (sl - eloff) (sc - ecoff -1)
          -- GHC Srcs end one after the end
          end = mkSrcLoc fname (el - eloff) (ec - ecoff)
traceTarget cc e@(L _ xp) fp fa = do
  tl <- fakeBaseLoc cc e
  traceTarget cc (L tl xp) fp fa

exprToModule :: CompileConfig -> String -> LHsBind GhcPs -> RProp -> [RExpr] -> RExpr
exprToModule CompConf {..} mname expr failing_prop failing_args =
  unlines $
    [ "module " ++ mname ++ " where"
    ]
      ++ importStmts
      ++ checkImports
      ++ lines failing_prop
      ++ [showUnsafe expr]
      ++ [ "",
           "main :: IO ()",
           "main = do r <- quickCheckWithResult (" ++ qcArgs ++ ") (" ++ pname ++ " fake_target " ++ unwords failing_args ++ ")",
           "          print (isSuccess r) "
         ]
  where
    pname = head (words failing_prop)

-- Report error prints the error and stops execution
reportError :: (HasCallStack, GhcMonad m, Outputable p) => p -> SourceError -> m b
reportError p e = do
  liftIO $ do
    putStrLn "FAILED!"
    putStrLn "UNEXPECTED EXCEPTION WHEN COMPILING CHECK:"
    putStrLn (showUnsafe p)
  printException e
  error "UNEXPECTED EXCEPTION"

-- When we want to compile only one parsed check
compileParsedCheck :: HasCallStack => CompileConfig -> EExpr -> IO Dynamic
compileParsedCheck cc expr = runGhc (Just libdir) $ do
  _ <- initGhcCtxt (cc {hole_lvl = 0})
  handleSourceError (reportError expr) $ dynCompileParsedExpr expr

-- Since initialization has some overhead, we have a special case for compiling
-- multiple checks at once.
compileParsedChecks :: HasCallStack => CompileConfig -> [EExpr] -> IO [CompileRes]
compileParsedChecks cc exprs = runGhc (Just libdir) $ do
  _ <- initGhcCtxt (cc {hole_lvl = 0})
  mapM
    ( \exp ->
        handleSourceError (reportError exp) $
          Right <$> dynCompileParsedExpr exp
    )
    exprs

-- Adapted from dynCompileExpr in InteractiveEval
dynCompileParsedExpr :: GhcMonad m => LHsExpr GhcPs -> m Dynamic
dynCompileParsedExpr parsed_expr = do
  let loc = getLoc parsed_expr
      to_dyn_expr =
        mkHsApp
          (L loc . HsVar noExtField . L loc $ getRdrName toDynName)
          parsed_expr
  hval <- compileParsedExpr to_dyn_expr
  return (unsafeCoerce# hval :: Dynamic)

genCandTys :: CompileConfig -> (RType -> RExpr -> RExpr) -> [RExpr] -> IO [RType]
genCandTys cc bcat cands = runGhc (Just libdir) $ do
  initGhcCtxt (cc {hole_lvl = 0})
  flags <- getSessionDynFlags
  catMaybes
    <$> mapM
      ( \c ->
          handleSourceError (const $ return Nothing) $
            Just . flip bcat c . showSDoc flags . ppr
              <$> exprType TM_Default c
      )
      cands

showUnsafe :: Outputable p => p -> String
showUnsafe = showSDocUnsafe . ppr

timeoutVal :: Int
timeoutVal = 1_000_000

-- Right True means that all the properties hold, while Right False mean that
-- There is some error or infinite loop.
-- Left bs indicates that the properties as ordered by bs are the ones that hold
runCheck :: Either [ValsAndRefs] Dynamic -> IO (Either [Bool] Bool)
runCheck (Left l) = return (Right False)
runCheck (Right dval) =
  -- Note! By removing the call to "isSuccess" in the buildCheckExprAtTy we
  -- can get more information, but then there can be a mismatch of *which*
  -- `Result` type it is... even when it's the same QuickCheck but compiled
  -- with different flags. Ugh. So we do it this way, since *hopefully*
  -- Bool will be the same (unless *base* was compiled differently, *UGGH*).
  case fromDynamic @(IO [Bool]) dval of
    Nothing ->
      do
        prDebug "wrong type!!"
        return (Right False)
    Just res ->
      -- We need to forkProcess here, since we might be evaulating
      -- non-yielding infinte expressions (like `last (repeat head)`), and
      -- since they never yield, we can't do forkIO and then stop that thread.
      -- If we could ensure *every library* was compiled with -fno-omit-yields
      -- we could use lightweight threads, but that is a very big restriction,
      -- especially if we want to later embed this into a plugin.
      do
        pid <- forkProcess (proc res)
        res <- timeout timeoutVal (getProcessStatus True False pid)
        case res of
          Just (Just (Exited ExitSuccess)) -> return $ Right True
          Nothing -> do
            signalProcess killProcess pid
            return $ Right False
          Just (Just (Exited (ExitFailure x)))
            | x < 0 ->
              -- If we have more than 8 props, we cannot tell
              -- which ones failed from the exit code.
              return $ Right False
          Just (Just (Exited (ExitFailure x))) ->
            return (Left $ bitToBools $ complement x)
          -- Anything else and we have no way to tell what went wrong.
          _ -> return $ Right False
  where
    proc action =
      do
        res <- action
        exitImmediately $
          if and res
            then ExitSuccess
            else -- We complement here, since ExitFailure 0 (i.e.
            -- all tests failed) is turned into ExitSuccess.
            -- We are limited to a maximum of 8 here, since the POSIX exit
            -- code is only 8 bits.

              ExitFailure $
                if length res <= 8
                  then complement $ boolsToBit res
                  else -1

compile :: CompileConfig -> RType -> IO CompileRes
compile cc str =
  runGhc (Just libdir) $ evalOrHoleFits cc str

compileAtType :: CompileConfig -> RExpr -> RType -> IO CompileRes
compileAtType cc str ty = compile cc ("((" ++ str ++ ") :: " ++ ty ++ ")")

showHF :: HoleFit -> String
showHF = showSDocUnsafe . pprPrefixOcc . hfId

readHole :: HoleFit -> (String, [RExpr])
readHole (RawHoleFit sdc) = (showSDocUnsafe sdc, [])
readHole hf@HoleFit {..} =
  ( showHF hf,
    map (showSDocUnsafe . ppr) hfMatches
  )
