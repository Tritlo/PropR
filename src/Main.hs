{-# LANGUAGE TemplateHaskell #-}
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

import Control.Concurrent
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Language.Haskell.TH (runIO, runQ, Lit(..), Exp (..))


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

-- We do this at compile-time to avoid later mess.
libDir :: Maybe String
libDir = Just $(runIO $
                 (LitE . StringL . head . lines) <$>
                 readCreateProcess (shell "ghc --print-libdir") "")

try :: String -> IO (Either [ValsAndRefs] Dynamic)
try = tryWLevel 1

tryAtType :: String -> String -> IO (Either [ValsAndRefs] Dynamic)
tryAtType = tryAtTypeWLvl 1

tryWLevel :: Int -> String -> IO (Either [ValsAndRefs] Dynamic)
tryWLevel lvl str = do
   -- libDir <- getLibDir
   r <- runGhc libDir $ evalOrHoleFits lvl str
--    print r
   return r

tryAtTypeWLvl :: Int -> String -> String -> IO (Either [ValsAndRefs] Dynamic)
tryAtTypeWLvl lvl str ty = tryWLevel lvl ("((" ++ str ++ ") :: " ++ ty ++ ")")


qcArgs = "(stdArgs { chatty = False, maxShrinks = 0})"
buildCheckExprAtTy :: [String] -> [String] -> String -> String -> String
buildCheckExprAtTy props context ty expr =
     unlines [ "let "
             , unlines (map ("    " ++) props)
             , unlines (map ("    " ++) context)
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

importStmts = ["import Prelude", "import Test.QuickCheck (quickCheckWithResult, Result(..), stdArgs, Args(..))"]
-- All the packages here need to be *globally* available. We should fix this
-- by wrapping it in e.g. a nix-shell or something.
packages = map toPkg ["base", "process", "QuickCheck"]

holeFlags = [ Opt_ShowHoleConstraints
            , Opt_ShowProvOfHoleFits
            , Opt_ShowTypeAppVarsOfHoleFits
            , Opt_ShowTypeAppOfHoleFits
            , Opt_ShowTypeOfHoleFits ]

synthesizeSatisfying :: Memo -> [String] -> String -> [String] -> IO [String]
synthesizeSatisfying = synthesizeSatisfyingWLevel 0 1

parM :: [IO a] -> IO [a]
-- parM actions = do mvs <- mapM start actions
--                   mapM readMVar mvs
--   where start action = do mv <- newEmptyMVar
--                           forkIO (action >>= putMVar mv)
--                           return mv
-- Safer
--modIORef = atomicModifyIORef'
parM = sequence
modIORef = modifyIORef
-- MEMOIZE
type SynthInput = (Int, Int, [String], String, [String])
type Memo = IORef (Map SynthInput (MVar [String]))


synthesizeSatisfyingWLevel :: Int -> Int -> Memo -> [String] -> String -> [String] -> IO [String]
synthesizeSatisfyingWLevel _       0     _       _  _     _ = return []
synthesizeSatisfyingWLevel lvl depth ioref context ty props = do
    let inp = (lvl, depth, context, ty, props)
    sM <- readIORef ioref
    case sM Map.!? inp of
        Just res -> do putStrLn $ "Found " ++ (show inp) ++ "!"
                       readMVar res
        Nothing -> do
            putStrLn $ "Synthesizing " ++ (show inp)
            nvar <- newEmptyMVar
            modIORef ioref (\m -> Map.insert inp nvar m)
            Left r <- tryAtTypeWLvl lvl (contextLet "_") ty
            case r of
              ((vals,refs):_) -> do
                  let rHoles = map readHole refs
                  rHVs <- parM $ map recur rHoles
                  let cands = (vals ++ (map wrap $ concat rHVs))
                  fits <- parM $ map (\v -> (>>=) (isFit v) (\r -> return (v,r))) cands
                  let res = map fst $ filter snd fits
                  putMVar nvar res
                  return res
              _ -> do putMVar nvar []
                      return []

  where isFit v = try (buildCheckExprAtTy props context ty v) >>= runCheck
        wrap p = "(" ++ p ++ ")"
        contextLet l = unlines ["let"
                               , unlines $ map ("    " ++)  context
                               , "in " ++ l]
        recur :: (String, [String]) -> IO [String]
        recur (e, []) = return [e]
        recur (e, [hole]) = do
          -- Weird, but we'll use the same structure for multiple holes later.
          -- No props for the hole.
          [holeFs] <- mapM ((flip (synthesizeSatisfyingWLevel 1 (depth-1) ioref context)) []) [hole]
          let cands = (map ((e ++ " ") ++) holeFs)
          return cands
        recur (e, holes@[h1,h2]) = do
          [h1fs,h2fs] <- mapM ((flip (synthesizeSatisfyingWLevel 1 (depth-1) ioref context)) []) holes

          let combs = (\a b -> a ++ " " ++ b) <$> h1fs <*> h2fs
              cands = map ((e ++ " ") ++) combs
          return cands
        recur _ = error "Multiple holes not implemented!"


-- This is probably slow, we should parse it properly.
readHole :: String -> (String, [String])
readHole str = case filter (\(r,left) -> left == "") (parseHole str) of
                -- here we should probably parse in a better way, i.e. pick
                -- the one with the most holes or something.
                (r,_):_ -> r
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
    let props = [ "propIsSymmetric f xs = f xs == f (reverse xs)"
                ] --, "propAlwaysPos f xs = f xs >= 0"]
        ty = "[Int] -> Int"
        context = ["zero = 0 :: Int", "one = 1 :: Int"]
    putStrLn "TARGET TYPE:"
    putStrLn $ "  "  ++ ty
    putStrLn "MUST SATISFY:"
    mapM_ (putStrLn . ("  " ++)) props
    putStrLn "IN CONTEXT:"
    mapM_ (putStrLn . ("  " ++)) context
    putStrLn "SYNTHESIZING..."
    memo <- newIORef (Map.empty)
    -- 2 is the number of additional holes at the top level,
    -- 3 is the depth.
    r <- synthesizeSatisfyingWLevel 2 3 memo context ty props
    case r of
        [] -> putStrLn "NO MATCH FOUND!"
        [xs] -> do putStrLn "FOUND MATCH:"
                   putStrLn xs
        xs -> do putStrLn $ "FOUND " ++ (show  $ length xs) ++" MATCHES:"
                 mapM_ putStrLn xs

    --putStrLn "ghc-synth!"
