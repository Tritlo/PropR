{-# LANGUAGE TypeApplications, RecordWildCards #-}
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

import Control.Monad (filterM, when)
import System.Timeout

import Control.Concurrent
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Language.Haskell.TH (runIO, runQ, Lit(..), Exp (..))

import GHC.Paths (libdir)

import System.Posix.Process
import System.Posix.Signals
import System.Exit
import System.Environment


config :: Int -> DynFlags -> DynFlags
config lvl sflags =
        (foldl gopt_unset sflags (Opt_OmitYields:holeFlags)) {
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
    when (null valsAndRefs) (printException err)
    --liftIO $ print $ valids
    --mapM_ (liftIO . print) valsAndRefs
    return $ Left $ valsAndRefs
    --(map (map (showSDocOneLine flags) . filter (isValid . showSDoc flags)) supp)

evalOrHoleFits :: Int -> String -> Ghc (Either [ValsAndRefs] Dynamic)
evalOrHoleFits lvl str = do
   initGhcCtxt lvl
   -- Then we can actually run the program!
   handleSourceError inspectException (dynCompileExpr str >>= (return . Right))


try :: String -> IO (Either [ValsAndRefs] Dynamic)
try = tryWLevel 1

tryAtType :: String -> String -> IO (Either [ValsAndRefs] Dynamic)
tryAtType = tryAtTypeWLvl 1

tryWLevel :: Int -> String -> IO (Either [ValsAndRefs] Dynamic)
tryWLevel lvl str = do
   r <- runGhc (Just libdir) $ evalOrHoleFits lvl str
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
             , "in ((sequence propsToCheck__) :: IO [Bool])"]
   where propNames = map (head . words) props
         propCheckExpr pname = "isSuccess <$> quickCheckWithResult "++qcArgs++" (" ++pname ++ " exprToCheck__)"
         propToLet p = "    " ++ p

-- The time we allow for a check to finish. Measured in microseconds.
timeoutVal :: Int
timeoutVal = 1000000
runCheck :: Either [ValsAndRefs] Dynamic -> IO Bool
runCheck (Left l) = return False
runCheck (Right dval) =
     -- Note! By removing the call to "isSuccess" in the buildCheckExprAtTy we
     -- can get more information, but then there can be a mismatch of *which*
     -- result type it is... ugh. So we do it this way, since the Bool will
     -- be the same.
     case fromDynamic @(IO [Bool]) dval of
         Nothing -> do pr_debug "wrong type!!"
                       return False
         Just res -> do pid <- forkProcess (proc res)
                        res <- timeout timeoutVal (getProcessStatus True False pid)
                        case res of
                          Just (Just (Exited ExitSuccess)) -> return True
                          Nothing -> do signalProcess killProcess pid
                                        return False
                          _ -> return False
  where proc action = do res <- action
                         exitImmediately $ if and res
                                           then ExitSuccess
                                           else (ExitFailure 1)

toPkg :: String -> PackageFlag
toPkg str = ExposePackage ("-package "++ str) (PackageArg str) (ModRenaming True [])


holeFlags = [ Opt_ShowHoleConstraints
            , Opt_ShowProvOfHoleFits
            , Opt_ShowTypeAppVarsOfHoleFits
            , Opt_ShowTypeAppOfHoleFits
            , Opt_ShowTypeOfHoleFits ]

synthesizeSatisfying :: Memo -> [String] -> String -> [String] -> IO [String]
synthesizeSatisfying = synthesizeSatisfyingWLevel 0 1

-- Safer
-- MEMOIZE
type SynthInput = (Int, Int, [String], String, [String])
type Memo = IORef (Map SynthInput [String])


parMap ::Int -> [IO a] -> IO [a]
parMap n xs | length xs < n = sequence xs
parMap n xs = do mvs <- mapM start cur
                 res <- mapM readMVar mvs
                 (res ++) <$> (parMap n rest)
  where (cur,rest) = splitAt n xs
        todo = zip cur $ repeat newEmptyMVar
        start act = do mv <- newEmptyMVar
                       forkIO (act >>= putMVar mv)
                       return mv

pr_debug :: String -> IO ()
pr_debug str = do dbg <- ("-fdebug" `elem`) <$> getArgs
                  if dbg then putStrLn str
                         else return ()


synthesizeSatisfyingWLevel :: Int -> Int -> Memo -> [String] -> String -> [String] -> IO [String]
synthesizeSatisfyingWLevel _    depth     _       _  _     _ | depth < 0 = return []
synthesizeSatisfyingWLevel lvl depth ioref context ty props = do
    let inp = (lvl, depth, context, ty, props)
    sM <- readIORef ioref
    case sM Map.!? inp of
        Just res -> do pr_debug $ "Found " ++ (show inp) ++ "!"
                       return res
        Nothing -> do
            pr_debug $ "Synthesizing " ++ (show inp)
            --nvar <- newEmptyMVar
            Left r <- tryAtTypeWLvl lvl (contextLet "_") ty
            case r of
              ((vals,refs):_) -> do
                  let rHoles = map readHole refs
                  rHVs <- sequence $ map recur rHoles
                  let cands = (vals ++ (map wrap $ concat rHVs))
                  res <-
                   if null props
                   then return cands
                   else do
                     let lv = length cands
                     putStrLn $ "CHECKING " ++ (show lv) ++ " CANDIDATES..."
                     pr_debug $ "Calculating FITS for" ++ show inp
                     fits <- sequence $
                       -- parMap 4 $
                       map
                       (\(i,v) ->
                           pr_debug ((show i) ++ "/"++ (show lv) ++ ": " ++ v) >>
                            (>>=) (isFit v) (\r -> return (v,r)))
                                $ zip [1..] cands
                     pr_debug $ (show inp) ++ " fits done!"
                     let res = map fst $ filter snd fits
                     return res
                  atomicModifyIORef' ioref (\m -> (Map.insert inp res m, ()))
                  return res
              _ -> do atomicModifyIORef' ioref (\m -> (Map.insert inp [] m, ()))
                      return []

  where isFit v = try bcat >>= runCheck
            where bcat = buildCheckExprAtTy props context ty v
        wrap p = "(" ++ p ++ ")"
        contextLet l = unlines ["let"
                               , unlines $ map ("    " ++)  context
                               , "in " ++ l]
        m = if depth <= 1 then 0 else 1
        recur :: (String, [String]) -> IO [String]
        recur (e, []) = return [e]
        recur (e, [hole]) = do
          -- Weird, but we'll use the same structure for multiple holes later.
          -- No props for the hole.
          pr_debug $ "Synthesizing for " ++ hole
          [holeFs] <- mapM ((flip (synthesizeSatisfyingWLevel m (depth-1) ioref context)) []) [hole]
          pr_debug $  hole ++ " Done!"
          let cands = (map ((e ++ " ") ++) holeFs)
          return cands
        recur (e, holes@[h1,h2]) = do
          pr_debug $ "Synthesizing for " ++ (show holes)
          [h1fs,h2fs] <- mapM ((flip (synthesizeSatisfyingWLevel m (depth-1) ioref context)) []) holes

          pr_debug $ show holes ++ " Done!"
          let combs = (\a b -> a ++ " " ++ b) <$> h1fs <*> h2fs
              cands = map ((e ++ " ") ++) combs
          return cands
        recur _ = error "More than 2 holes is not supported!"


-- This is probably slow, we should parse it properly.
readHole :: String -> (String, [String])
readHole str = case filter (\(r,left) -> left == "") (parseHole str) of
                -- here we should probably parse in a better way, i.e. pick
                -- the one with the most holes or something.
                (r,_):_ -> r
                o -> error ("No parse: \n"
                           ++ str ++ "\nGot: " ++ (show $ parseHole str))
  where po = char '('
        pc = char ')'
        any = satisfy $ const True
        hole = string "_ :: " >> many any
        parseHole = readP_to_S $ do e1 <- manyTill any (char ' ')
                                    hs <- sepBy (between po pc hole) (char ' ')
                                    return (e1, hs)


importStmts = [ "import Prelude hiding (id, ($), ($!), asTypeOf)"
              , "import Test.QuickCheck (quickCheckWithResult, Result(..), stdArgs, Args(..), isSuccess, (==>))"
              ]
-- All the packages here need to be *globally* available. We should fix this
-- by wrapping it in e.g. a nix-shell or something.
packages = map toPkg ["base", "process", "QuickCheck" ]

hasDebug :: IO Bool
hasDebug = ("-fdebug" `elem`) <$> getArgs

data SynthFlags = SFlgs { synth_holes :: Int
                        , synth_depth :: Int
                        , synth_debug :: Bool}


getFlags :: IO SynthFlags
getFlags = do args <- Map.fromList . (map (break (== '='))) <$> getArgs
              let synth_holes = case args Map.!? "-fholes" of
                                    Just r | not (null r) -> read (tail r)
                                    Nothing -> 2
                  synth_depth = case args Map.!? "-fdepth" of
                                    Just r | not (null r) -> read (tail r)
                                    Nothing -> 1
                  synth_debug = "-fdebug" `Map.member` args
              when (synth_holes > 2) (error "MORE THAN 2 HOLES NOT SUPPORTED!")
              when (synth_holes < 0) (error "NUMBER OF HOLES CANNOT BE NEGATIVE!")
              when (synth_depth < 0) (error "DEPTH CANNOT BE NEGATIVE!")
              return $ SFlgs {..}


main :: IO ()
main = do
    SFlgs {..} <- getFlags
    let props = [ "prop_IsSymmetric f xs = f xs == f (reverse xs)"
                , "prop_Bin f = f [] == 0 || f [] == 1"
                , "prop_not_const f = not ((f []) == f [1,2,3])"
                ]
        ty = "[Int] -> Int"
        context = ["zero = 0 :: Int", "one = 1 :: Int"]
    putStrLn "SCOPE:"
    mapM_ (putStrLn . ("  " ++)) importStmts
    putStrLn "TARGET TYPE:"
    putStrLn $ "  "  ++ ty
    putStrLn "MUST SATISFY:"
    mapM_ (putStrLn . ("  " ++)) props
    putStrLn "IN CONTEXT:"
    mapM_ (putStrLn . ("  " ++)) context
    putStrLn "PARAMETERS:"
    putStrLn $ "  MAX HOLES: "  ++ (show synth_holes)
    putStrLn $ "  MAX DEPTH: "  ++ (show synth_depth)
    putStrLn "SYNTHESIZING..."
    memo <- newIORef (Map.empty)
    -- 2 is the number of additional holes at the top level,
    -- 3 is the depth. Takes 60ish minutes on my system, but works!
    r <- synthesizeSatisfyingWLevel synth_holes synth_depth memo context ty props
    case r of
        [] -> putStrLn "NO MATCH FOUND!"
        [xs] -> do putStrLn "FOUND MATCH:"
                   putStrLn xs
        xs -> do putStrLn $ "FOUND " ++ (show  $ length xs) ++" MATCHES:"
                 mapM_ putStrLn xs

    --putStrLn "ghc-synth!"
