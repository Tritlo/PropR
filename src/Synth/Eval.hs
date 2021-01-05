{-# LANGUAGE RecordWildCards #-}
module Synth.Eval where

-- GHC API
import GHC
import DynFlags
import ErrUtils
-- import SrcLoc
import HscTypes

import Outputable hiding (char)

import Bag

import GHC.Paths (libdir)

import Control.Monad (when)
import Control.Monad.IO.Class

import Data.Dynamic
import Data.Maybe




-- Configuration and GHC setup

holeFlags = [ Opt_ShowHoleConstraints
            , Opt_ShowProvOfHoleFits
            , Opt_ShowTypeAppVarsOfHoleFits
            , Opt_ShowTypeAppOfHoleFits
            , Opt_ShowTypeOfHoleFits ]

config :: Int -> DynFlags -> DynFlags
config lvl sflags =
        (foldl gopt_unset sflags (Opt_OmitYields:holeFlags)) {
               maxValidHoleFits = Nothing,
               maxRefHoleFits = Nothing,
               refLevelHoleFits = Just lvl }

-- UTIL

output :: Outputable p => [p] -> Ghc ()
output p = do
    flags <- getSessionDynFlags
    mapM_ (liftIO . print . showSDoc flags . ppr) p

----

data CompileConfig = CompConf { importStmts :: [String]
                              , packages :: [String]
                              , hole_lvl :: Int}
   deriving (Show, Eq, Ord)


toPkg :: String -> PackageFlag
toPkg str = ExposePackage ("-package "++ str) (PackageArg str) (ModRenaming True [])

initGhcCtxt :: CompileConfig -> Ghc ()
initGhcCtxt CompConf{..} = do
   flags <- (config hole_lvl) <$> getSessionDynFlags
     --`dopt_set` Opt_D_dump_json
   -- First we have to add "base" to scope
   toLink <- setSessionDynFlags (flags {packageFlags = (packageFlags flags)
                                                     ++ ( map toPkg packages)})
   -- "If you are not doing linking or doing static linking, you can ignore the list of packages returned."
   --(hsc_dynLinker <$> getSession) >>= liftIO . (flip extendLoadedPkgs toLink)
   -- Then we import the prelude and add it to the context
   imports <- mapM ( fmap IIDecl . parseImportDecl) importStmts
   getContext >>= setContext . (imports ++)


type ValsAndRefs = ([String], [String])
type CompileRes = Either [ValsAndRefs] Dynamic


-- Extract
getHoleFitsFromError :: SourceError -> Ghc CompileRes
getHoleFitsFromError err = do
    flags <- getSessionDynFlags
    let isHole = allBag holeImp $ (errDocImportant . errMsgDoc) <$> (srcErrorMessages err)
           where holeImp = all isHoleMsg . map (showSDoc flags)
                 isHoleMsg m = take (length holeMsg) m == holeMsg
                   where holeMsg = "Found hole:"
        supp = bagToList $ (errDocSupplementary . errMsgDoc) <$> (srcErrorMessages err)
        isValid ('V':'a':'l':'i':'d':_:xs) =
            case xs of
                'h':'o':'l':'e':_ -> True
                'r':'e':'f':'i':'n':'e':'m':'e':'n':'t':_ -> True
                _ -> False
        isValid _ = False
        toMb [] = Nothing
        toMb [x] = Just x
        toMb o = error (show o)
        valids :: [Maybe String]
        valids = map (toMb . filter isValid . map (showSDoc flags)) supp
        spl v = (map (dropWhile (== ' ')) vals, map (dropWhile (== ' ')) rfs)
          where (vals,rfs) = case break isValid (tail v) of
                                (v,r:rfs) -> (v,rfs)
                                (v, []) -> (v, [])
        valsAndRefs = map spl $ map lines $ catMaybes valids
    when (null valsAndRefs && (not isHole)) (printException err)
    return $ Left $ valsAndRefs


evalOrHoleFits :: CompileConfig -> String -> Ghc CompileRes
evalOrHoleFits cc str = do
   initGhcCtxt cc
   -- Then we can actually run the program!
   handleSourceError getHoleFitsFromError (dynCompileExpr str >>= (return . Right))

compileChecks :: CompileConfig -> [String] -> IO [CompileRes]
compileChecks cc exprs = runGhc (Just libdir) $ do
    initGhcCtxt (cc {hole_lvl = 0})
    mapM (handleSourceError (\e -> printException e >> return (Left []))
          . fmap Right . dynCompileExpr ) exprs


-- try :: CompileConfig -> String -> IO CompileRes
-- try = tryWLevel

-- tryAtType :: Compile-String -> String -> IO CompileRes
-- tryAtType = tryAtTypeWLvl 1

compile :: CompileConfig -> String -> IO CompileRes
compile cc str = do
   r <- runGhc (Just libdir) $ evalOrHoleFits cc str
   return r

compileAtType :: CompileConfig -> String -> String -> IO CompileRes
compileAtType cc str ty = compile cc ("((" ++ str ++ ") :: " ++ ty ++ ")")
