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

import System.Process

import Data.Dynamic
import Data.List
import Test.QuickCheck

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

inspectException :: SourceError -> Ghc (Either [ValsAndRefs] Dynamic)
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
    --liftIO $ print $ valids
    --mapM_ (liftIO . print) valsAndRefs
    return $ Left $ valsAndRefs
    --(map (map (showSDocOneLine flags) . filter (isValid . showSDoc flags)) supp)

evalOrHoleFits :: String -> Ghc (Either [ValsAndRefs] Dynamic)
evalOrHoleFits str = do
   initGhcCtxt
   -- Then we can actually run the program!
   handleSourceError inspectException (dynCompileExpr str >>= (return . Right))

getLibDir :: IO (Maybe String)
getLibDir = do ld <- readCreateProcess (shell "ghc --print-libdir") ""
               return $ case lines ld of
                          [ld] -> Just ld
                          _ -> Nothing

try :: String -> IO (Either [ValsAndRefs] Dynamic)
try str = do
   libDir <- getLibDir
   r <- runGhc libDir $ evalOrHoleFits str
   print r
   return r

tryAtType :: String -> String -> IO (Either [ValsAndRefs] Dynamic)
tryAtType str ty = try ("((" ++ str ++ ") :: " ++ ty ++ ")")


tyToTry = "[Int] -> Int"
props = ["propIsSum f xs = f xs == sum xs"]
propNames = map (head . words) props
propCheckExpr pname = "quickCheckResult (" ++ pname ++ " exprToCheck__)"
propToLet p = "    " ++ p

buildCheckExprAtTy expr ty =
     unlines [ "let "
             , unlines (map ("    " ++) props)
             , "    exprToCheck__ = (" ++ expr ++ " :: " ++ ty ++ ")"
             , "    propsToCheck__ = [" ++ (intercalate "," $ map propCheckExpr propNames) ++ "]"
             , "in ((sequence propsToCheck__) :: IO [Result])"]


runCheck :: Either [ValsAndRefs] Dynamic -> IO Bool
runCheck (Left _) = return False
runCheck (Right dval) =
     case fromDynamic dval of
         Nothing -> return False
         Just res -> do r <- (res :: IO [Result])
                        print r
                        return $ all isSuccess r

toPkg :: String -> PackageFlag
toPkg str = ExposePackage ("-package "++ str) (PackageArg str) (ModRenaming True [])

importStmts = ["import Prelude", "import Test.QuickCheck"]
-- All the packages here need to be *globally* available. We should fix this
-- by wrapping it in e.g. a nix-shell or something.
packages = map toPkg ["base", "process", "QuickCheck"]

holeFlags = [ Opt_ShowHoleConstraints
            , Opt_ShowMatchesOfHoleFits
            , Opt_ShowProvOfHoleFits
            , Opt_ShowTypeAppVarsOfHoleFits
            , Opt_ShowTypeAppOfHoleFits
            , Opt_ShowTypeOfHoleFits ]

main :: IO ()
main = do
    -- try "let thisIsAnExtremelyLongNameOfAFunctionThatIAmHopingWillBreakAndOhMyGodIHaveToMakeItEvenLongerICannotBelieveThisIsItEvenPossible False = True in (_ (_ :: Bool)) :: Bool"
    -- res <- try "True" -- base case. Returns Right <<Bool>>
    --res <- try "succ" -- does not work, we need a concrete type if we want
                        -- a dynamic  i.e. something Typeable. Returns Left []
    -- res <- try "succ :: Bool -> Bool" -- returns Right <<Bool -> Bool>>
    -- res2 <- tryAtType "succ" "Bool -> Bool" -- returns Right <<Bool -> Bool>>
    -- print res2
    -- res <- tryAtType exprToTry tyToTry
    -- print res
    r2 <- try (buildCheckExprAtTy "product" "[Int] -> Int")
    print r2
    r3 <- runCheck r2
    print r3

    --putStrLn "ghc-synth!"
