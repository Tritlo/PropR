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

dropPrefix :: String -> String -> String
dropPrefix (p:ps) (s:ss) | p == s = dropPrefix ps ss
dropPrefix _ s = s

startsWith :: String -> String -> Bool
startsWith [] _ = True
startsWith (p:ps) (s:ss) | p == s = startsWith ps ss
startsWith _ _ = False

-- Extract
getHoleFitsFromError :: SourceError -> Ghc CompileRes
getHoleFitsFromError err = do
    flags <- getSessionDynFlags
    dbg <- liftIO $ ("-fdebug" `elem`) <$> getArgs
    when dbg $ printException err
    let isHole = allBag holeImp $ (errDocImportant . errMsgDoc) <$> (srcErrorMessages err)
           where holeImp = all isHoleMsg . map (showSDoc flags)
                 isHoleMsg m = take (length holeMsg) m == holeMsg
                   where holeMsg = "Found hole:"
        supp = bagToList $ (errDocSupplementary . errMsgDoc) <$> (srcErrorMessages err)
        isValid s =
             (startsWith "Valid hole" s) || (startsWith "Valid refinement" s)
        toMb [] = Nothing
        toMb [x] = Just x
        toMb o = error (show o)
        valids :: [Maybe String]
        valids = map (toMb . filter isValid . map (showSDoc flags)) supp
        spl v = (vals', rfs')
          where ((va,vals),(ra,rfs)) =
                    case break (startsWith "Valid refinement") v of
                         (v:vs, r:rfs) -> ((v,vs),(r,rfs))
                         (v:vs, []) -> ((v,vs), ([],[]))
                         ([], r:rfs) -> (([],[]),(r,rfs))
                -- We need to do a bit of stuff in case the  hole fit itself
                -- ends up on the same line as the message
                v' = dropPrefix "Valid hole fits include " va
                vals' = if null v' then clean vals else v':clean vals
                r' = dropPrefix "Valid refinement hole fits include " ra
                rfs' = if null r' then clean rfs else r':clean rfs
                leading_spaces =
                     case (vals,rfs) of
                         (v:_,_) -> lsp v
                         (_,r:_) -> lsp r
                         _ -> 0
                lsp = length . takeWhile (== ' ')
                -- We need to clean the fits so that fits that don't fit in one
                -- line are parsed as one fit and not others
                clean = map (unwords .  words)
                      -- ^ remove extra spaces between holes
                      . map unwords . gr2
                      -- ^ group where the one that follows has an extra leading
                      -- space, i.e. is a start of a hung one
                      . map unwords . gr
                      -- ^ group those that have even more leading spaces, i.e.
                      -- are hung below (so they are part of the one above)
                      . map (drop leading_spaces)
                      -- ^ remove leading spaces from all
                gr = groupBy c
                  where c (' ':_) (' ':_) = True
                        c _ _ = False
                gr2 = groupBy c
                  where c _ (' ':_) = True
                        c _ _ = False

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
    mapM (\exp ->
         handleSourceError (\e ->
          do liftIO $ do putStrLn "FAILED!"
                         putStrLn "UNEXPECTED EXCEPTION WHEN COMPILING CHECK:"
                         putStrLn exp
             printException e
             error "UNEXPECTED EXCEPTION")
          $ fmap Right $ dynCompileExpr exp ) exprs


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
