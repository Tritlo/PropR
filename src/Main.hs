{-# LANGUAGE TypeApplications, RecordWildCards #-}
module Main where


import Data.Maybe

import System.Process
import System.IO

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

import System.Posix.Process
import System.Posix.Signals
import System.Exit
import System.Environment

import Synth.Eval

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
     -- `Result` type it is... even when it's the same QuickCheck but compiled
     -- with different flags. Ugh. So we do it this way, since *hopefully*
     -- Bool will be the same (unless *base* was compiled differently, *UGGH*).
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


-- MEMOIZE
type SynthInput = (CompileConfig, Int, [String], String, [String])
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

putStr' :: String -> IO ()
putStr' str = putStr str >> hFlush stdout

synthesizeSatisfying :: CompileConfig
                     -> Int -> Memo -> [String]
                     -> String -> [String] -> IO [String]
synthesizeSatisfying _    depth     _       _  _     _ | depth < 0 = return []
synthesizeSatisfying cc depth ioref context ty props = do
  let inp = (cc, depth, context, ty, props)
  sM <- readIORef ioref
  case sM Map.!? inp of
    Just res -> pr_debug ("Found " ++ (show inp) ++ "!") >> return res
    Nothing -> do
      pr_debug $ "Synthesizing " ++ (show inp)
      --nvar <- newEmptyMVar
      Left r <- compileAtType cc (contextLet "_") ty
      case r of
        ((vals,refs):_) -> do
          let rHoles = map readHole refs
          rHVs <- sequence $ map recur rHoles
          let cands = (vals ++ (map wrap $ concat rHVs))
          res <- if null props then return cands
           else do
             -- This ends the "GENERATING CANDIDATES..." message.
             putStrLn "DONE!"
             let lv = length cands
             putStrLn $ "GENERATED " ++ show lv ++ " CANDIDATES!"
             putStr' "COMPILING CANDIDATE CHECKS..."
             checks <- zip cands <$> (compileChecks cc $ map bcat cands)
             putStrLn "DONE!"
             let to_check =  checks
             putStr' ("CHECKING " ++ (show lv) ++ " CANDIDATES...")
             fits <- mapM
               (\(i,(v,c)) ->
                  do pr_debug ((show i) ++ "/"++ (show lv) ++ ": " ++ v)
                     r <- runCheck c
                     return (v,r)) $ zip [1..] to_check
             putStrLn $ "DONE!"
             pr_debug $ (show inp) ++ " fits done!"
             let res = map fst $ filter snd fits
             return res
          atomicModifyIORef' ioref (\m -> (Map.insert inp res m, ()))
          return res
        _ -> do atomicModifyIORef' ioref (\m -> (Map.insert inp [] m, ()))
                return []

  where isFit v = compile (cc {hole_lvl=0}) (bcat v) >>= runCheck
        bcat = buildCheckExprAtTy props context ty
        wrap p = "(" ++ p ++ ")"
        contextLet l = unlines ["let"
                               , unlines $ map ("    " ++)  context
                               , "in " ++ l]
        cc' = if depth <= 1 then (cc {hole_lvl=0}) else (cc {hole_lvl=1})
        recur :: (String, [String]) -> IO [String]
        recur (e, []) = return [e]
        recur (e, [hole]) = do
          -- Weird, but we'll use the same structure for multiple holes later.
          -- No props for the hole.
          pr_debug $ "Synthesizing for " ++ hole
          [holeFs] <- mapM ((flip (synthesizeSatisfying cc' (depth-1) ioref context)) []) [hole]
          pr_debug $  hole ++ " Done!"
          let cands = (map ((e ++ " ") ++) holeFs)
          return cands
        recur (e, holes@[h1,h2]) = do
          pr_debug $ "Synthesizing for " ++ (show holes)
          [h1fs,h2fs] <- mapM ((flip (synthesizeSatisfying cc' (depth-1) ioref context)) []) holes

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

-- All the packages here need to be *globally* available. We should fix this
-- by wrapping it in e.g. a nix-shell or something.
pkgs = ["base", "process", "QuickCheck" ]

imports = [ "import Prelude hiding (id, ($), ($!), asTypeOf)"
          , "import Test.QuickCheck (quickCheckWithResult, Result(..), stdArgs, Args(..), isSuccess, (==>))"
          ]

compConf :: CompileConfig
compConf = CompConf { importStmts = imports
                    , packages = pkgs
                    , hole_lvl = 0}
main :: IO ()
main = do
    SFlgs {..} <- getFlags
    let cc = compConf {hole_lvl=synth_holes}
    let props = [ "prop_IsSymmetric f xs = f xs == f (reverse xs)"
                , "prop_Bin f = f [] == 0 || f [] == 1"
                , "prop_not_const f = not ((f []) == f [1,2,3])"
                ]
        ty = "[Int] -> Int"
        context = ["zero = 0 :: Int", "one = 1 :: Int"]
    putStrLn "SCOPE:"
    mapM_ (putStrLn . ("  " ++)) imports
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
    putStr' "GENERATING CANDIDATES..."
    r <- synthesizeSatisfying cc synth_depth memo context ty props
    case r of
        [] -> putStrLn "NO MATCH FOUND!"
        [xs] -> do putStrLn "FOUND MATCH:"
                   putStrLn xs
        xs -> do putStrLn $ "FOUND " ++ (show  $ length xs) ++" MATCHES:"
                 mapM_ putStrLn xs

    --putStrLn "ghc-synth!"
