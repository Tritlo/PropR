{-# LANGUAGE TypeApplications, RecordWildCards, TupleSections #-}
module Main where


import Data.Maybe

import System.Process
import System.IO
import System.Environment ( getArgs )

import Data.Dynamic
import Data.List
import Data.Maybe

import Text.ParserCombinators.ReadP

import Control.Monad (filterM, when)

import Control.Concurrent
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import System.CPUTime
import Text.Printf

import Synth.Eval
import Synth.Repair
import Synth.Check
import Synth.Util
import Synth.Flatten
import Synth.Diff
import Data.Tree

import Trace.Hpc.Mix (BoxLabel(ExpBox))


import GhcPlugins (unLoc, GenLocated(..), getLoc)

-- The time we allow for a check to finish. Measured in microseconds.

type SynthInput = (CompileConfig, Int, [String], String, [String])
type Memo = IORef (Map SynthInput [String])


synthesizeSatisfying :: CompileConfig -> Int -> Memo -> [String]
                     -> [String] -> String -> IO [String]
synthesizeSatisfying _    depth     _       _  _     _ | depth < 0 = return []
synthesizeSatisfying cc depth ioref context props ty = do
  let inp = (cc, depth, context, ty, props)
  sM <- readIORef ioref
  case sM Map.!? inp of
    Just res -> pr_debug ("Found " ++ (show inp) ++ "!") >> return res
    Nothing -> do
      pr_debug $ "Synthesizing " ++ (show inp)
      mono_ty <- monomorphiseType cc ty
      Left r <- compileAtType cc (contextLet context "_") ty
      case r of
        ((vals,refs):_) -> do
          let rHoles = map readHole refs
          rHVs <- sequence $ map recur rHoles
          let cands = ((map showHF vals) ++ (map wrap $ concat rHVs))
              lv = length cands
          res <- if null props then return cands else do
             -- This ends the "GENERATING CANDIDATES..." message.
             case mono_ty of
               Nothing -> do
                 putStrLn "FAILED!"
                 putStrLn $ "COULD NOT MONOMORPHISE " ++ ty
                 putStrLn "THIS MEANS QUICKCHECKS CANNOT BE DONE!"
                 return []
               Just mty -> do
                 putStrLn "DONE!"
                 putStrLn $ "GENERATED " ++ show lv ++ " CANDIDATES!"
                 putStr' "COMPILING CANDIDATE CHECKS..."
                 let imps' = checkImports ++ importStmts cc
                     cc' = (cc {hole_lvl=0, importStmts=imps'})
                     to_check_exprs = map (bcat mty) cands
                 -- Doesn't work, since the types are too polymorphic, and if
                 -- the target type cannot be monomorphised, the fits will
                 -- be too general for QuickCheck
                 -- to_check_exprs <-
                 --    case mono_ty of
                 --        Just typ -> return $ map (bcat typ) cands
                 --        Nothing -> genCandTys cc' bcat cands
                 to_check <- zip cands <$> compileChecks cc' to_check_exprs
                 putStrLn "DONE!"
                 putStr' ("CHECKING " ++ (show lv) ++ " CANDIDATES...")
                 fits <- mapM
                   (\(i,(v,c)) ->
                      do pr_debug ((show i) ++ "/"++ (show lv) ++ ": " ++ v)
                         (v,) . (Right True ==) <$> runCheck c) $ zip [1..] to_check
                 putStrLn "DONE!"
                 pr_debug $ (show inp) ++ " fits done!"
                 let res = map fst $ filter snd fits
                 return res
          atomicModifyIORef' ioref (\m -> (Map.insert inp res m, ()))
          return res
        _ -> do atomicModifyIORef' ioref (\m -> (Map.insert inp [] m, ()))
                return []

  where
    bcat = buildCheckExprAtTy props context
    wrap p = "(" ++ p ++ ")"
    cc' = if depth <= 1 then (cc {hole_lvl=0}) else (cc {hole_lvl=1})
    recur :: (String, [String]) -> IO [String]
    recur (e, []) = return [e]
    recur (e, holes) = do
      pr_debug $ "Synthesizing for " ++ show holes
      holeFs <- mapM (synthesizeSatisfying cc' (depth-1) ioref context []) holes
      pr_debug $ (show holes) ++ " Done!"
      -- We synthesize for each of the holes, and then produce ALL COMBINATIONS
      return $
       if any null holeFs then []
       else map ((e ++ " ") ++) $ map unwords $ combinations holeFs
      where combinations :: [[String]] -> [[String]]
            combinations [] = [[]]
            -- List monad magic
            combinations (c:cs) = do x <- c
                                     xs <- combinations cs
                                     return (x:xs)

data SynthFlags = SFlgs { synth_holes :: Int
                        , synth_depth :: Int
                        , synth_debug :: Bool
                        , repair_target :: Maybe FilePath}


getFlags :: IO SynthFlags
getFlags = do args <- Map.fromList . (map (break (== '='))) <$> getArgs
              let synth_holes = case args Map.!? "-fholes" of
                                    Just r | not (null r) -> read (tail r)
                                    Nothing -> 2
                  synth_depth = case args Map.!? "-fdepth" of
                                    Just r | not (null r) -> read (tail r)
                                    Nothing -> 1
                  synth_debug = "-fdebug" `Map.member` args
                  repair_target = tail <$> args Map.!? "-ftarget"

              when (synth_holes < 0) (error "NUMBER OF HOLES CANNOT BE NEGATIVE!")
              when (synth_depth < 0) (error "DEPTH CANNOT BE NEGATIVE!")
              return $ SFlgs {..}

-- All the packages here need to be *globally* available. We should fix this
-- by wrapping it in e.g. a nix-shell or something.
pkgs = ["base", "process", "QuickCheck" ]

imports = [
    "import Prelude hiding (id, ($), ($!), asTypeOf)"
  ]



compConf :: CompileConfig
compConf = CompConf { importStmts = imports
                    , packages = pkgs
                    , hole_lvl = 0}

showTime :: Integer -> String
showTime time = if res > 1000
                then (printf "%.2f" ((fromIntegral res * 1e-3) :: Double)) ++ "s"
                else show res ++ "ms"
  where res :: Integer
        res = (floor $ (fromIntegral time) * 1e-9)

time :: IO a -> IO (Integer, a)
time act = do start <- getCPUTime
              r <- act
              done <- getCPUTime
              return (done - start, r)

main :: IO ()
main = do
    SFlgs {..} <- getFlags
    let cc = compConf {hole_lvl=synth_holes}
    [toFix] <- filter (not . (==) "-f" . take 2 ) <$> getArgs
    (cc, context, mod, probs) <- moduleToProb cc toFix repair_target
    let ((target, wrong_prog, ty, props):_) = if null probs
                                              then error "NO TARGET FOUND!"
                                              else probs
    putStrLn "TARGET:"
    putStrLn ("  `" ++ target ++ "` in " ++ toFix)
    putStrLn "SCOPE:"
    mapM_ (putStrLn . ("  " ++)) (importStmts cc)
    putStrLn "TARGET TYPE:"
    putStrLn $ "  "  ++ ty
    putStrLn "MUST SATISFY:"
    mapM_ (putStrLn . ("  " ++)) props
    putStrLn "IN CONTEXT:"
    mapM_ (putStrLn . ("  " ++)) context
    putStrLn "PARAMETERS:"
    putStrLn $ "  MAX HOLES: "  ++ (show synth_holes)
    putStrLn $ "  MAX DEPTH: "  ++ (show synth_depth)
    putStrLn "PROGRAM TO REPAIR: "
    putStrLn wrong_prog
    putStrLn "FAILING PROPS:"
    failing_props <- failingProps cc props context ty wrong_prog
    mapM (putStrLn . ("  " ++)) failing_props
    putStrLn "COUNTER EXAMPLES:"
    counter_examples <- mapM (propCounterExample cc context ty wrong_prog) failing_props
    mapM (putStrLn . (++) "  " . (\t -> if (t == "") then "<none>" else t) . unwords) $ catMaybes counter_examples
    eMap <-
       (Map.fromList . map (\e -> (getLoc e, showUnsafe e))  . flattenExpr) <$>
          runJustParseExpr cc wrong_prog

    let hasCE (p, Just ce) = Just (p, ce)
        hasCE _ = Nothing
    reses <- mapM (uncurry $ traceTarget cc wrong_prog)
                 $ mapMaybe hasCE $ zip failing_props counter_examples
    let trcs = map (map (\(s,r) ->
                             (s, eMap Map.!? (mkInteractive s),
                              r, maximum $ map snd r)) . flatten) $ catMaybes reses
    putStrLn "TRACE OF COUNTER EXAMPLES:"
    mapM (putStrLn . unlines . map ((++) "  " . show)) trcs
    putStr' "REPAIRING..."
    (t, fixes) <- time $ repair cc props context ty wrong_prog
    putStrLn $ "DONE! (" ++ showTime t ++ ")"
    putStrLn "REPAIRS:"
    fbs <- mapM (getFixBinds cc) fixes
    mapM (putStrLn . unlines . map ((++) "  ") . lines) $
       map (concatMap (prettyFix True) . snd . applyFixes mod) fbs
    putStrLn "SYNTHESIZING..."
    memo <- newIORef (Map.empty)
    putStr' "GENERATING CANDIDATES..."
    (t, r) <- time $ synthesizeSatisfying cc synth_depth memo context props ty
    putStrLn $ "DONE! (" ++ showTime t ++ ")"
    case r of
        [] -> putStrLn "NO MATCH FOUND!"
        [xs] -> do putStrLn "FOUND MATCH:"
                   putStrLn xs
        xs -> do putStrLn $ "FOUND " ++ (show  $ length xs) ++" MATCHES:"
                 mapM_ putStrLn xs


