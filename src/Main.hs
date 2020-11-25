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

try :: String -> IO ()
try str =
    do  res <- runGhc libDir $ do
               flags <- getSessionDynFlags
               let p :: Outputable p => p -> Ghc ()
                   p = liftIO . print . showSDoc flags . ppr
               toLink <- setSessionDynFlags (flags {packageFlags = (packageFlags flags) ++ packages})
               (hsc_dynLinker <$> getSession) >>= liftIO . (flip extendLoadedPkgs toLink)
               imports <- mapM ( fmap IIDecl . parseImportDecl) importStmts
               getContext >>= setContext . (imports ++)

               env <- getSession
               r <- parseExpr str
               val <- compileParsedExpr r
               return $ show val
        print res



main :: IO ()
main = do
    try "_ :: Bool"
    --putStrLn "ghc-synth!"
