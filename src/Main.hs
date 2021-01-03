module Main where

import Parser
import Lexer
import Outputable hiding (char)
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

import Text.ParserCombinators.ReadP

import Control.Monad (filterM)

config :: Int -> DynFlags -> DynFlags
config lvl sflags =
        (foldl gopt_unset sflags holeFlags) {
               maxValidHoleFits = Nothing,
               maxRefHoleFits = Nothing,
               refLevelHoleFits = Just lvl }

initGhcCtxt :: Int -> Ghc ()
initGhcCtxt lvl = do
   flags <- (config lvl) <$> getSessionDynFlags
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
    -- printException err
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
          where (vals,rfs) = case break isValid (tail v) of
                                (v,r:rfs) -> (v,rfs)
                                (v, []) -> (v, [])
        valsAndRefs = map spl $ map lines $ catMaybes valids
    --liftIO $ print $ valids
    --mapM_ (liftIO . print) valsAndRefs
    return $ Left $ valsAndRefs
    --(map (map (showSDocOneLine flags) . filter (isValid . showSDoc flags)) supp)

evalOrHoleFits :: Int -> String -> Ghc (Either [ValsAndRefs] Dynamic)
evalOrHoleFits lvl str = do
   initGhcCtxt lvl
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
   r <- runGhc libDir $ evalOrHoleFits 1 str
--    print r
   return r

tryAtType :: String -> String -> IO (Either [ValsAndRefs] Dynamic)
tryAtType str ty = try ("((" ++ str ++ ") :: " ++ ty ++ ")")

tryWLevel :: Int -> String -> IO (Either [ValsAndRefs] Dynamic)
tryWLevel lvl str = do
   libDir <- getLibDir
   r <- runGhc libDir $ evalOrHoleFits lvl str
   return r

tryAtTypeWLvl :: Int -> String -> String -> IO (Either [ValsAndRefs] Dynamic)
tryAtTypeWLvl lvl str ty = tryWLevel lvl ("((" ++ str ++ ") :: " ++ ty ++ ")")



tyToTry = "[Int] -> Int"

qcArgs = "(stdArgs { chatty = False, maxShrinks = 0})"
buildCheckExprAtTy :: [String] -> String -> String -> String
buildCheckExprAtTy props ty expr =
     unlines [ "let "
             , unlines (map ("    " ++) props)
             , "    exprToCheck__ = (" ++ expr ++ " :: " ++ ty ++ ")"
             , "    propsToCheck__ = [" ++ (intercalate "," $ map propCheckExpr propNames) ++ "]"
             , "in ((sequence propsToCheck__) :: IO [Result])"]
   where propNames = map (head . words) props
         propCheckExpr pname = "quickCheckWithResult "++qcArgs++" (" ++ pname ++ " exprToCheck__)"
         propToLet p = "    " ++ p


runCheck :: Either [ValsAndRefs] Dynamic -> IO Bool
runCheck (Left _) = return False
runCheck (Right dval) =
     case fromDynamic dval of
         Nothing -> return False
         Just res -> do r <- (res :: IO [Result])
                        -- print r
                        return $ all isSuccess r

toPkg :: String -> PackageFlag
toPkg str = ExposePackage ("-package "++ str) (PackageArg str) (ModRenaming True [])

importStmts = ["import Prelude", "import Test.QuickCheck"]
-- All the packages here need to be *globally* available. We should fix this
-- by wrapping it in e.g. a nix-shell or something.
packages = map toPkg ["base", "process", "QuickCheck"]

holeFlags = [ Opt_ShowHoleConstraints
            , Opt_ShowProvOfHoleFits
            , Opt_ShowTypeAppVarsOfHoleFits
            , Opt_ShowTypeAppOfHoleFits
            , Opt_ShowTypeOfHoleFits ]

synthesizeSatisfying :: Int -> String -> [String] -> IO [String]
synthesizeSatisfying lvl ty props = do
    Left r <- tryAtTypeWLvl lvl "_" ty
    case r of
      ((vals,refs):_) -> do
          let rHoles = map readHole refs
          rHoleVals <- concat <$> mapM recur rHoles
          filterM isFit (vals ++ rHoleVals)
      _ -> return []

  where isFit v = try (buildCheckExprAtTy props ty v) >>= runCheck
        recur :: (String, [String]) -> IO [String]
        recur (e, []) = return [e]
        recur (e, [hole]) = do
          -- Weird, but we'll use the same structure for multiple holes later.
          -- No props for the hole.
          (holeFs:_) <- mapM ((flip (synthesizeSatisfying 0)) []) [hole]
          return (map ((e ++ " ") ++) holeFs)
        recur _ = error "Multiple holes not implemented!"


-- This is probably slow, we should parse it properly.
readHole :: String -> (String, [String])
readHole str = case filter (\(r,left) -> left == "") (parseHole str) of
                [(r,_)] -> r
                o -> error ("No parse" ++ show o)
  where po = char '('
        pc = char ')'
        any = satisfy $ const True
        hole = string "_ :: " >> many any
        parseHole = readP_to_S $ do e1 <- manyTill any (char ' ')
                                    hs <- sepBy (between po pc hole) (char ' ')
                                    return (e1, hs)




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

    -- r2 <- try (buildCheckExprAtTy props "[Int] -> Int" "product")
    -- print r2
    -- r3 <- runCheck r2
    -- print r3
    let props = ["propIsSymmetric f xs = f xs == f (reverse xs)"]
                 --"propAlwaysPos f xs = f xs >= 0"]
        ty = "[Int] -> Int"
    putStrLn "TARGET TYPE:"
    putStrLn $ "  "  ++ ty
    putStrLn "MUST SATISFY:"
    mapM_ (putStrLn . ("  " ++)) props
    putStrLn "SYNTHESIZING..."
    r <- synthesizeSatisfying 1 ty props
    case r of
        [] -> putStrLn "NO MATCH FOUND!"
        [xs] -> do putStrLn "FOUND MATCH:"
                   putStrLn xs
        xs -> do putStrLn "FOUND MATCHES:"
                 mapM_ putStrLn xs

    --putStrLn "ghc-synth!"
