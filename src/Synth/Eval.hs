{-# LANGUAGE RecordWildCards, TypeApplications #-}
module Synth.Eval where

import Synth.Types

-- GHC API
import GHC
import DynFlags
import ErrUtils ( errMsgDoc, errDocSupplementary, errDocImportant )
import HscTypes ( SourceError, srcErrorMessages )

import Outputable hiding (char)

import Bag

import GHC.Paths (libdir)

import Control.Monad (when)
import Control.Monad.IO.Class ( liftIO )

import Data.Dynamic
import Data.Maybe
import Data.List
import Data.Function (on)
import Data.Either

import TysWiredIn (unitTy)
import GhcPlugins (substTyWith, PluginWithArgs(..), StaticPlugin(..)
                  , occName, OccName(..), fsLit, mkOccNameFS, concatFS
                  , HscEnv(hsc_IC), InteractiveContext(ic_default)
                  , mkVarUnqual, getRdrName)

import Synth.Plugin
import Data.IORef
import TcHoleErrors (TypedHole (..), HoleFit(..))
import Constraint (Ct(..), holeOcc)
import Data.Data


import System.Posix.Process
import System.Posix.Signals
import System.Exit
import System.Environment
import System.Timeout

import Synth.Util

import Data.Bits (complement)

-- Configuration and GHC setup

holeFlags = [ Opt_ShowHoleConstraints
            , Opt_ShowProvOfHoleFits
            , Opt_ShowTypeAppVarsOfHoleFits
            , Opt_ShowTypeAppOfHoleFits
            , Opt_ShowTypeOfHoleFits ]

setFlags = [Opt_Hpc]

config :: Int -> DynFlags -> DynFlags
config lvl sflags =
        (flip (foldl gopt_set) setFlags) $
          (foldl gopt_unset sflags (Opt_OmitYields:holeFlags)) {
               maxValidHoleFits = Nothing,
               maxRefHoleFits = Nothing,
               refLevelHoleFits = Just lvl }

-- UTIL

output :: Outputable p => [p] -> Ghc ()
output p = do
    flags <- getSessionDynFlags
    dbg <- liftIO hasDebug
    when dbg $
       mapM_ (liftIO . print . showSDoc flags . ppr) p

----

data CompileConfig = CompConf { importStmts :: [String]
                              , packages :: [String]
                              , hole_lvl :: Int}
   deriving (Show, Eq, Ord)


toPkg :: String -> PackageFlag
toPkg str = ExposePackage ("-package "++ str) (PackageArg str) (ModRenaming True [])

initGhcCtxt :: CompileConfig -> Ghc (IORef [(TypedHole, [HoleFit])])
initGhcCtxt CompConf{..} = do
   flags <- (config hole_lvl) <$> getSessionDynFlags
     --`dopt_set` Opt_D_dump_json
   -- First we have to add "base" to scope
   plugRef <- liftIO $ newIORef []
   let flags' = flags { packageFlags = (packageFlags flags)
                                    ++ (map toPkg packages)
                      , staticPlugins = sPlug:(staticPlugins flags) }
       sPlug = StaticPlugin $ PluginWithArgs { paArguments = []
                                             , paPlugin = synthPlug plugRef}
   toLink <- setSessionDynFlags flags'
   -- "If you are not doing linking or doing static linking, you can ignore the list of packages returned."
   --(hsc_dynLinker <$> getSession) >>= liftIO . (flip extendLoadedPkgs toLink)
   -- Then we import the prelude and add it to the context
   imports <- mapM ( fmap IIDecl . parseImportDecl) importStmts
   getContext >>= setContext . (imports ++)
   return plugRef


type ValsAndRefs = ([HoleFit], [HoleFit])
type CompileRes = Either [ValsAndRefs] Dynamic

-- By integrating with a hole fit plugin, we can extract the fits (with all
-- the types and everything directly, instead of having to parse the error
-- message)
getHoleFitsFromError :: IORef ([(TypedHole, [HoleFit])])
                     -> SourceError -> Ghc (Either [ValsAndRefs] b)
getHoleFitsFromError plugRef err = do
    flags <- getSessionDynFlags
    dbg <- liftIO hasDebug
    when dbg $ printException err
    res <- liftIO $ readIORef plugRef
    when (null res) (printException err)
    let gs = groupBy (sameHole `on` fst) res
        allFitsOfHole ((th, f):rest) = (th, concat $ f:(map snd rest))
        valsAndRefs = map (partition part . snd) $ map allFitsOfHole gs
    return $ Left valsAndRefs
  where part (RawHoleFit _) = True
        part (HoleFit {..}) = hfRefLvl <= 0
        sameHole :: TypedHole -> TypedHole -> Bool
        sameHole (TyH {tyHCt = Just (CHoleCan {cc_hole = h1})})
                 (TyH {tyHCt = Just (CHoleCan {cc_hole = h2})}) =
                 (holeOcc h1) == (holeOcc h2)
        sameHole _ _ = False

monomorphiseType :: CompileConfig -> RType -> IO (Maybe RType)
monomorphiseType cc ty = do
   runGhc (Just libdir) $
       do _ <- initGhcCtxt cc
          flags <- getSessionDynFlags
          let pp = showSDoc flags . ppr
          handleSourceError (const $ return Nothing)
            ((Just . pp . mono) <$> (exprType TM_Default ("undefined :: " ++ ty)))

  where mono ty = substTyWith tvs (replicate (length tvs) unitTy) base_ty
          where (tvs, base_ty) = splitForAllTys ty
        -- ^ We take a leaf from QuickCheck's book and default all ambiguous
        -- foralls to the simplest one, unit.

evalOrHoleFits :: CompileConfig -> RExpr -> Ghc CompileRes
evalOrHoleFits cc str = do
   plugRef <- initGhcCtxt cc
   -- Then we can actually run the program!
   handleSourceError (getHoleFitsFromError plugRef)
                     (dynCompileExpr str >>= (return . Right))


-- Report error prints the error and stops execution
reportError :: GhcMonad m => RExpr -> SourceError -> m b
reportError expr e = do liftIO $ do putStrLn "FAILED!"
                                    putStrLn "UNEXPECTED EXCEPTION WHEN COMPILING CHECK:"
                                    putStrLn expr
                        printException e
                        error "UNEXPECTED EXCEPTION"

-- When we want to compile only one check
compileCheck :: CompileConfig -> RExpr -> IO Dynamic
compileCheck cc expr = runGhc (Just libdir) $ do
    _ <- initGhcCtxt (cc {hole_lvl = 0})
    handleSourceError (reportError expr) $ dynCompileExpr expr

-- Since initialization has some overhead, we have a special case for compiling
-- multiple checks at once.
compileChecks :: CompileConfig -> [RExpr] -> IO [CompileRes]
compileChecks cc exprs = runGhc (Just libdir) $ do
    _ <- initGhcCtxt (cc {hole_lvl = 0})
    mapM (\exp -> handleSourceError (reportError exp)
          $ fmap Right $ dynCompileExpr exp ) exprs

genCandTys :: CompileConfig -> (RType -> RExpr -> RExpr) -> [RExpr] -> IO [RType]
genCandTys cc bcat cands = runGhc (Just libdir) $ do
    initGhcCtxt (cc {hole_lvl = 0})
    flags <- getSessionDynFlags
    catMaybes <$>
        mapM (\c -> handleSourceError (const $ return Nothing) $
                Just . flip bcat c . showSDoc flags . ppr
                    <$> exprType TM_Default c) cands

showUnsafe :: Outputable p => p -> String
showUnsafe = showSDocUnsafe . ppr

timeoutVal :: Int
timeoutVal = 1000000



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
        do pr_debug "wrong type!!"
           return (Right False)
      Just res ->
        -- We need to forkProcess here, since we might be evaulating
        -- non-yielding infinte expressions (like `last (repeat head)`), and
        -- since they never yield, we can't do forkIO and then stop that thread.
        -- If we could ensure *every library* was compiled with -fno-omit-yields
        -- we could use lightweight threads, but that is a very big restriction,
        -- especially if we want to later embed this into a plugin.
        do pid <- forkProcess (proc res)
           res <- timeout timeoutVal (getProcessStatus True False pid)
           case res of
             Just (Just (Exited ExitSuccess)) -> return $ Right True
             Nothing -> do signalProcess killProcess pid
                           return $ Right False
             Just (Just (Exited (ExitFailure x))) | x < 0 ->
                -- If we have more than 8 props, we cannot tell
                -- which ones failed from the exit code.
                return $ Right False
             Just (Just (Exited (ExitFailure x))) ->
                 return (Left $ bitToBools $ complement x)
  where proc action =
          do res <- action
             exitImmediately $
                if and res
                then ExitSuccess
                 -- We complement here, since ExitFailure 0 (i.e.
                 -- all tests failed) is turned into ExitSuccess.
                 -- We are limited to a maximum of 8 here, since the POSIX exit
                 -- code is only 8 bits.
                else ExitFailure $ if length res <= 8
                                   then (complement $ boolsToBit res)
                                   else -1

compile :: CompileConfig -> RType -> IO CompileRes
compile cc str = do
   r <- runGhc (Just libdir) $ evalOrHoleFits cc str
   return r

compileAtType :: CompileConfig -> RExpr -> RType -> IO CompileRes
compileAtType cc str ty = compile cc ("((" ++ str ++ ") :: " ++ ty ++ ")")


showHF :: HoleFit -> String
showHF = showSDocUnsafe . pprPrefixOcc . hfId

readHole :: HoleFit -> (String, [RExpr])
readHole (RawHoleFit sdc) = (showSDocUnsafe sdc, [])
readHole hf@HoleFit{..} =
    (showHF hf,
     map (showSDocUnsafe . ppr) hfMatches)
