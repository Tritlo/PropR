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
import Bag

import Data.Maybe

import Control.Monad.IO.Class


importStmts = ["import Prelude"]
packages = [ExposePackage "base" (PackageArg "base") (ModRenaming True [])]
-- Found from running `ghc --lib-dir`
libDir = Just "/nix/store/ijx5zivd823kp4qzb773fmg7a2qcf7ix-ghc-8.10.1/lib/ghc-8.10.1"
holeFlags = [ Opt_ShowHoleConstraints
            , Opt_ShowMatchesOfHoleFits
            , Opt_ShowProvOfHoleFits
            , Opt_ShowTypeAppVarsOfHoleFits
            , Opt_ShowTypeAppOfHoleFits
            , Opt_ShowTypeOfHoleFits ]

config :: DynFlags -> DynFlags
config sflags =
        (foldl gopt_unset sflags holeFlags) {
               maxValidHoleFits = Nothing,
               maxRefHoleFits = Nothing,
               refLevelHoleFits = Just 2 }

initGhcCtxt :: Ghc ()
initGhcCtxt = do
   flags <- config <$> getSessionDynFlags
     --`dopt_set` Opt_D_dump_json
   -- First we have to add "base" to scope
   toLink <- setSessionDynFlags (flags {packageFlags = (packageFlags flags) ++ packages})
   -- "If you are not doing linking or doing static linking, you can ignore the list of packages returned."
   --(hsc_dynLinker <$> getSession) >>= liftIO . (flip extendLoadedPkgs toLink)
   -- Then we import the prelude and add it to the context
   imports <- mapM ( fmap IIDecl . parseImportDecl) importStmts
   getContext >>= setContext . (imports ++)

output :: Outputable p => [p] -> Ghc ()
output p = do
    flags <- getSessionDynFlags
    mapM_ (liftIO . print . showSDoc flags . ppr) p

type ValsAndRefs = ([String], [String])

inspectException :: SourceError -> Ghc (Either [ValsAndRefs] HValue)
inspectException err = do
    flags <- getSessionDynFlags
    printException err
    let supp = bagToList $ (errDocSupplementary . errMsgDoc) <$> (srcErrorMessages err)
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
          where (vals,r:rfs) = break isValid (tail v)
        valsAndRefs = map spl $ map lines $ catMaybes valids
    liftIO $ print $ valids
    mapM_ (liftIO . print) valsAndRefs
    return $ Left $ valsAndRefs
    --(map (map (showSDocOneLine flags) . filter (isValid . showSDoc flags)) supp)

evalOrHoleFits :: String -> Ghc (Either [ValsAndRefs] HValue)
evalOrHoleFits str = do
   initGhcCtxt
   -- Then we can actually run the program!
   handleSourceError inspectException (compileExpr str >>= (return . Right))

try :: String -> IO ()
try str = do
   r <- runGhc libDir $ evalOrHoleFits str
   print r



main :: IO ()
main = do
    try "let thisIsAnExtremelyLongNameOfAFunctionThatIAmHopingWillBreakAndOhMyGodIHaveToMakeItEvenLongerICannotBelieveThisIsItEvenPossible False = True in (_ (_ :: Bool)) :: Bool"
    try "True"
    --putStrLn "ghc-synth!"
