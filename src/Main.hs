{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Endemic.Check (buildSuccessCheck, checkImports)
import Endemic.Diff
import Endemic.Eval
import Endemic.Repair (detranslate, getExprFitCands, translate)
import Endemic.Search.Genetic.Configuration (mkDefaultConf)
import Endemic.Search.Genetic.GenMonad (runGenMonad)
import Endemic.Search.Genetic.Search (geneticSearchPlusPostprocessing)
import Endemic.Traversals (replaceExpr)
import Endemic.Types
import Endemic.Util
import GHC (HsExpr (HsLet), NoExtField (..))
import GhcPlugins (noLoc)
import System.Environment (getArgs)

-- The time we allow for a check to finish. Measured in microseconds.

type SynthInput = (CompileConfig, Int, [String], String, [String])

type Memo = IORef (Map String [String])

synthesizeSatisfying ::
  CompileConfig ->
  Int ->
  Memo ->
  [String] ->
  [String] ->
  String ->
  IO [String]
synthesizeSatisfying _ depth _ _ _ _ | depth < 0 = return []
synthesizeSatisfying cc depth ioref context props ty = do
  let inp = (cc, depth, context, ty, props)
  sM <- readIORef ioref
  case sM Map.!? (show inp) of
    Just res -> logStr INFO ("Found " ++ show inp ++ "!") >> return res
    Nothing -> do
      logStr INFO $ "Synthesizing " ++ show inp
      mono_ty <- monomorphiseType cc ty
      Left r <- compileAtType cc (contextLet context "_") ty
      case r of
        ((vals, refs) : _) -> do
          let rHoles = map readHole refs
          rHVs <- mapM recur rHoles
          let cands = map showHF vals ++ map wrap (concat rHVs)
              lv = length cands
          res <-
            if null props
              then return cands
              else do
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
                        cc' = (cc {hole_lvl = 0, importStmts = imps'})
                        to_check_probs = map (rprob mty) cands
                    to_check_exprs <-
                      mapM
                        ( fmap buildSuccessCheck
                            . translate cc
                        )
                        to_check_probs
                    -- Doesn't work, since the types are too polymorphic, and if
                    -- the target type cannot be monomorphised, the fits will
                    -- be too general for QuickCheck
                    -- to_check_exprs <-
                    --    case mono_ty of
                    --        Just typ -> return $ map (bcat typ) cands
                    --        Nothing -> genCandTys cc' bcat cands
                    to_check <- zip cands <$> compileParsedChecks cc' to_check_exprs
                    putStrLn "DONE!"
                    putStr' ("CHECKING " ++ show lv ++ " CANDIDATES...")
                    fits <-
                      mapM
                        ( \(i, (v, c)) ->
                            do
                              logStr INFO (show i ++ "/" ++ show lv ++ ": " ++ v)
                              (v,) . (Right True ==) <$> runCheck c
                        )
                        $ zip [1 ..] to_check
                    putStrLn "DONE!"
                    logStr INFO $ show inp ++ " fits done!"
                    let res = map fst $ filter snd fits
                    return res
          atomicModifyIORef' ioref (\m -> (Map.insert (show inp) res m, ()))
          return res
        _ -> do
          atomicModifyIORef' ioref (\m -> (Map.insert (show inp) [] m, ()))
          return []
  where
    rprob ty cand = RProb props context "" ty cand
    wrap p = "(" ++ p ++ ")"
    cc' = if depth <= 1 then (cc {hole_lvl = 0}) else (cc {hole_lvl = 1})
    recur :: (String, [String]) -> IO [String]
    recur (e, []) = return [e]
    recur (e, holes) = do
      logStr INFO $ "Synthesizing for " ++ show holes
      holeFs <- mapM (synthesizeSatisfying cc' (depth -1) ioref context []) holes
      logStr INFO $ show holes ++ " Done!"
      -- We synthesize for each of the holes, and then produce ALL COMBINATIONS
      return $
        if any null holeFs
          then []
          else map (((e ++ " ") ++) . unwords) (combinations holeFs)
      where
        combinations :: [[String]] -> [[String]]
        combinations [] = [[]]
        -- List monad magic
        combinations (c : cs) = do
          x <- c
          xs <- combinations cs
          return (x : xs)

data SynthFlags = SFlgs
  { synth_holes :: Int,
    synth_depth :: Int,
    synth_debug :: Bool,
    repair_target :: Maybe FilePath
  }

getFlags :: IO SynthFlags
getFlags = do
  args <- Map.fromList . map (break (== '=')) <$> getArgs
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
pkgs = ["base", "process", "QuickCheck"]

imports =
  [ "import Prelude hiding (id, ($), ($!), asTypeOf)"
  ]

compConf :: CompileConfig
compConf =
  defaultConf
    { importStmts = imports,
      packages = pkgs,
      hole_lvl = 0
    }

main :: IO ()
main = do
  SFlgs {..} <- getFlags
  let cc = compConf {hole_lvl = synth_holes}
  [toFix] <- filter (not . (==) "-" . take 1) <$> getArgs
  (cc, mod, probs) <- moduleToProb cc toFix repair_target
  let (tp@EProb {..} : _) = if null probs then error "NO TARGET FOUND!" else probs
      rp@RProb {..} = detranslate tp
  putStrLn "TARGET:"
  putStrLn ("  `" ++ r_target ++ "` in " ++ toFix)
  putStrLn "SCOPE:"
  mapM_ (putStrLn . ("  " ++)) (importStmts cc)
  putStrLn "TARGET TYPE:"
  putStrLn $ "  " ++ r_ty
  putStrLn "MUST SATISFY:"
  mapM_ (putStrLn . ("  " ++)) r_props
  putStrLn "IN CONTEXT:"
  mapM_ (putStrLn . ("  " ++)) r_ctxt
  putStrLn "PARAMETERS:"
  putStrLn $ "  MAX HOLES: " ++ show synth_holes
  putStrLn $ "  MAX DEPTH: " ++ show synth_depth
  putStrLn "PROGRAM TO REPAIR: "
  putStrLn $ showUnsafe e_prog

  no_par_gen <- ("--no-par-gen" `elem`) <$> getArgs
  no_par_rep <- ("--no-par-rep" `elem`) <$> getArgs
  no_rep_interpreted <- ("--no-rep-interpreted" `elem`) <$> getArgs
  let CompConf {pseudoGenConf = gc, repConf = rp} = cc
      cc' =
        cc
          { pseudoGenConf = gc {genPar = not no_par_gen},
            repConf =
              rp
                { repParChecks = not no_par_rep,
                  repUseInterpreted = not no_rep_interpreted
                }
          }
  putStrLn "REPAIRING..."
  expr_fit_cands <- collectStats $ getExprFitCands cc $ noLoc $ HsLet NoExtField e_ctxt $ noLoc undefVar
  let gconf = mkDefaultConf 64 50 tp cc' expr_fit_cands
  (t, fixes) <- time $ runGenMonad gconf 69420 (geneticSearchPlusPostprocessing)
  let newProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
      fbs = map getFixBinds newProgs
  mapM_ (putStrLn . concatMap (colorizeDiff . ppDiff) . snd . applyFixes mod) fbs
  reportStats' INFO
  putStrLn $ "DONE! (" ++ showTime t ++ ")"
