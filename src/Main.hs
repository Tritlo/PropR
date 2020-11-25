module Main where

import Parser
import Lexer
import Outputable
import StringBuffer
import FastString
import SrcLoc
import GHC
import ErrUtils
import DynFlags

import HscTypes
import PrelNames
import Linker

import Control.Monad.IO.Class


importStmts = ["import Prelude"]
packages = [ExposePackage "base" (PackageArg "base") (ModRenaming True [])]
-- Found from running `ghc --lib-dir`
libDir = Just "/nix/store/ijx5zivd823kp4qzb773fmg7a2qcf7ix-ghc-8.10.1/lib/ghc-8.10.1"

initGhcCtxt :: Ghc ()
initGhcCtxt = do
   sflags <- getSessionDynFlags
   let flags =  sflags --`dopt_set` Opt_D_dump_json
   -- First we have to add "base" to scope
   toLink <- setSessionDynFlags (flags {packageFlags = (packageFlags flags) ++ packages})
   -- "If you are not doing linking or doing static linking, you can ignore the list of packages returned."
   --(hsc_dynLinker <$> getSession) >>= liftIO . (flip extendLoadedPkgs toLink)
   -- Then we import the prelude and add it to the context
   imports <- mapM ( fmap IIDecl . parseImportDecl) importStmts
   getContext >>= setContext . (imports ++)

output :: Outputable p => p -> Ghc ()
output p = do
    flags <- getSessionDynFlags
    (liftIO . print . showSDoc flags . ppr) p

inspectException :: SourceError -> Ghc ()
inspectException err = do
    flags <- getSessionDynFlags
    output $ pprErrMsgBagWithLoc $ srcErrorMessages err
    printException err

try :: String -> IO ()
try str = runGhc libDir $ do
   initGhcCtxt
   -- Then we can actually run the program!
   handleSourceError inspectException (dynCompileExpr str >>= (liftIO . print))



main :: IO ()
main = do
    try "_ :: Bool"
    try "True"
    --putStrLn "ghc-synth!"
