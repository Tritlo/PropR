{-# LANGUAGE RecordWildCards #-}
module Synth.Eval where

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

import System.Environment ( getArgs )

import Data.Dynamic
import Data.Maybe
import Data.List
import Data.Function (on)

import TysWiredIn (unitTy)
import GhcPlugins (substTyWith, PluginWithArgs(..), StaticPlugin(..)
                  , occName, OccName(..), fsLit, mkOccNameFS, concatFS
                  , HscEnv(hsc_IC), InteractiveContext(ic_default))

import Synth.Plugin
import Data.IORef
import TcHoleErrors (TypedHole (..), HoleFit(..))
import Constraint (Ct(..), holeOcc)
import Data.Data

-- Configuration and GHC setup

holeFlags = [ Opt_ShowHoleConstraints
            , Opt_ShowProvOfHoleFits
            , Opt_ShowTypeAppVarsOfHoleFits
            , Opt_ShowTypeAppOfHoleFits
            , Opt_ShowTypeOfHoleFits ]

config :: Int -> DynFlags -> DynFlags
config lvl sflags =
        ((foldl gopt_unset sflags [Opt_OmitYields]) {
               maxValidHoleFits = Nothing,
               maxRefHoleFits = Nothing,
               refLevelHoleFits = Just lvl })

-- UTIL

output :: Outputable p => [p] -> Ghc ()
output p = do
    flags <- getSessionDynFlags
    dbg <- liftIO $ ("-fdebug" `elem`) <$> getArgs
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

dropPrefix :: String -> String -> String
dropPrefix (p:ps) (s:ss) | p == s = dropPrefix ps ss
dropPrefix _ s = s

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith (p:ps) (s:ss) | p == s = startsWith ps ss
startsWith _ _ = False

-- By integrating with a hole fit plugin, we can extract the fits (with all
-- the types and everything directly, instead of having to parse the error
-- message)
getHoleFitsFromError :: IORef ([(TypedHole, [HoleFit])])
                     -> SourceError -> Ghc CompileRes
getHoleFitsFromError plugRef err = do
    flags <- getSessionDynFlags
    dbg <- liftIO $ ("-fdebug" `elem`) <$> getArgs
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

monomorphiseType :: CompileConfig -> String -> IO (Maybe String)
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

evalOrHoleFits :: CompileConfig -> String -> Ghc CompileRes
evalOrHoleFits cc str = do
   plugRef <- initGhcCtxt cc
   -- Then we can actually run the program!
   handleSourceError (getHoleFitsFromError plugRef)
                     (dynCompileExpr str >>= (return . Right))

compileChecks :: CompileConfig -> [String] -> IO [CompileRes]
compileChecks cc exprs = runGhc (Just libdir) $ do
    _ <- initGhcCtxt (cc {hole_lvl = 0})
    mapM (\exp ->
         handleSourceError (\e ->
          do liftIO $ do putStrLn "FAILED!"
                         putStrLn "UNEXPECTED EXCEPTION WHEN COMPILING CHECK:"
                         putStrLn exp
             printException e
             error "UNEXPECTED EXCEPTION")
          $ fmap Right $ dynCompileExpr exp ) exprs

genCandTys :: CompileConfig -> (String -> String -> String) -> [String] -> IO [String]
genCandTys cc bcat cands = runGhc (Just libdir) $ do
    initGhcCtxt (cc {hole_lvl = 0})
    flags <- getSessionDynFlags
    catMaybes <$>
        mapM (\c -> handleSourceError (const $ return Nothing) $
                Just . flip bcat c . showSDoc flags . ppr
                    <$> exprType TM_Default c) cands


getAST :: CompileConfig -> String -> IO [LHsExpr GhcPs]
getAST cc str = do
   r <- runGhc (Just libdir) $ exprAST cc str
   return r

exprAST :: CompileConfig -> String -> Ghc [LHsExpr GhcPs]
exprAST cc str = do
   plugRef <- initGhcCtxt cc
   -- Then we can actually run the program!
   ~(Just (L l r)) <- handleSourceError (\err -> printException err >>= return (return Nothing))
                               (parseExpr str >>= (return . Just))
   flags <- getSessionDynFlags

   -- Make sure we don't do too much defaulting by setting `default ()`
   -- Note: I think this only applies to the error we would be generating,
   -- I think if we replace the UnboundVar with a suitable var of the right
   -- it would work... it just makes the in-between output a bit confusing.
   env <- getSession
   setSession (env {hsc_IC = (hsc_IC env) {ic_default = Just []}})

   let shw = showSDoc flags $ ppr_expr r
   liftIO $ putStrLn shw
   let replaced = replaceWithHoles (L l r)
   output replaced
   res <- mapM (\c -> handleSourceError (\e -> printException e >> return False)
                                        (compileParsedExpr c >> return True))
               replaced
   output res
   return replaced

replaceWithHoles :: LHsExpr GhcPs -> [LHsExpr GhcPs]
replaceWithHoles (L loc (HsApp x l r)) = rl ++ rr
  where rl = map (\e -> L loc (HsApp x e r)) $ replaceWithHoles l
        rr = map (\e -> L loc (HsApp x l e)) $ replaceWithHoles r
replaceWithHoles (L loc (HsVar x (L _ v))) =
    [(L loc (HsUnboundVar x (TrueExprHole name)))]
  where (ns,fs) = (occNameSpace (occName v), occNameFS (occName v))
        name = mkOccNameFS ns (concatFS $ (fsLit "_"):[fs])

replaceWithHoles (L loc (HsPar x l)) =
    map (L loc . HsPar x) $ replaceWithHoles l
replaceWithHoles (L loc (ExprWithTySig x l t)) =
    map (L loc . flip (ExprWithTySig x) t) $ replaceWithHoles l
replaceWithHoles e = []


compile :: CompileConfig -> String -> IO CompileRes
compile cc str = do
   r <- runGhc (Just libdir) $ evalOrHoleFits cc str
   return r

compileAtType :: CompileConfig -> String -> String -> IO CompileRes
compileAtType cc str ty = compile cc ("((" ++ str ++ ") :: " ++ ty ++ ")")


showHF :: HoleFit -> String
showHF = showSDocUnsafe . pprPrefixOcc . hfId

readHole :: HoleFit -> (String, [String])
readHole (RawHoleFit sdc) = (showSDocUnsafe sdc, [])
readHole hf@HoleFit{..} =
    (showHF hf,
     map (showSDocUnsafe . ppr) hfMatches)
