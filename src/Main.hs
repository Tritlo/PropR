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
import System.Directory (doesFileExist,createDirectory)
import System.IO
import Data.Time.LocalTime (utc)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime,defaultTimeLocale)

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
  case sM Map.!? show inp of
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
                    logStr INFO " Failed to generate !"
                    logStr INFO $ "could not monomorphise " ++ ty ++ " - this means QuickCheck cannot be done"
                    return []
                  Just mty -> do
                    logStr DEBUG "Done monomorphising!"
                    logStr DEBUG $ "generated " ++ show lv ++ " candaidates!"
                    logStr DEBUG $ "compiling candidate checks..."
                    let imps' = checkImports ++ importStmts cc
                        cc'' = (cc {hole_lvl = 0, importStmts = imps'})
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
                    to_check <- zip cands <$> compileParsedChecks cc'' to_check_exprs
                    logStr DEBUG "done compiling candidates"
                    logStr DEBUG ("checking " ++ show lv ++ " candidates...")
                    fits <-
                      mapM
                        ( \(i, (v, c)) ->
                            do
                              logStr INFO (show i ++ "/" ++ show lv ++ ": " ++ v)
                              (v,) . (Right True ==) <$> runCheck c
                        )
                        $ zip [1 :: Int ..] to_check
                    logStr DEBUG  "Done Checking candidates"
                    logStr INFO $ show inp ++ " fits done!"
                    let res = map fst $ filter snd fits
                    return res
          atomicModifyIORef' ioref (\m -> (Map.insert (show inp) res m, ()))
          return res
        _ -> do
          atomicModifyIORef' ioref (\m -> (Map.insert (show inp) [] m, ()))
          return []
  where
    rprob ty' cand = RProb props context "" ty' cand
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
        _ -> 2
      synth_depth = case args Map.!? "-fdepth" of
        Just r | not (null r) -> read (tail r)
        _ -> 1
      synth_debug = "-fdebug" `Map.member` args
      repair_target = tail <$> args Map.!? "-ftarget"

  when (synth_holes < 0) (error "Number of Holes cannot be negative")
  when (synth_depth < 0) (error "Depth cannot be negative")
  return $ SFlgs {..}

-- All the packages here need to be *globally* available. We should fix this
-- by wrapping it in e.g. a nix-shell or something.
pkgs :: [[Char]]
pkgs = ["base", "process", "QuickCheck"]

imports :: [[Char]]
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
  let cc_orig = compConf {hole_lvl = synth_holes}
  [toFix] <- filter (not . (==) "-" . take 1) <$> getArgs
  (cc, modul, probs) <- moduleToProb cc_orig toFix repair_target
  let (tp@EProb {..} : _) = if null probs then error "NO TARGET FOUND!" else probs
      RProb {..} = detranslate tp
  logStr INFO "Target:"
  logStr INFO ("  `" ++ r_target ++ "` in " ++ toFix)
  logStr DEBUG "Scope:"
  mapM_ (logStr DEBUG . ("  " ++)) (importStmts cc)
  logStr INFO "Target Type:"
  logStr INFO $ "  " ++ r_ty
  logStr INFO "Must Satisfy:"
  mapM_ (logStr INFO . ("  " ++)) r_props
  logStr DEBUG "In Context:"
  mapM_ (logStr DEBUG . ("  " ++)) r_ctxt
  logStr DEBUG "Parameters:"
  logStr DEBUG $ "  Max Holes: " ++ show synth_holes
  logStr DEBUG $ "  Max Depth: " ++ show synth_depth
  logStr INFO "Program to Repair: "
  logStr INFO $ showUnsafe e_prog

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
  logStr INFO "Repairing..."
  expr_fit_cands <- collectStats $ getExprFitCands cc $ noLoc $ HsLet NoExtField e_ctxt $ noLoc undefVar
  let gconf = mkDefaultConf 64 50 tp cc' expr_fit_cands
  (t, fixes) <- time $ runGenMonad gconf 69420 geneticSearchPlusPostprocessing
  let newProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
      fbs = map getFixBinds newProgs
  -- Here we write the found solutions to respective files, we just number them 1 .. n
  formTime <- formattedTime
  -- TODO: Add a Configurable prefix for the output directory, e.g. /tmp/
  let outputDirectory = "./output-patches-" ++ formTime
  createDirectory outputDirectory
  let prettyPrinted = map (concatMap (ppDiff) . snd . applyFixes modul) fbs
  savePatchesToFiles prettyPrinted outputDirectory
  mapM_ (putStrLn . concatMap (colorizeDiff . ppDiff) . snd . applyFixes modul) fbs
  reportStats' INFO
  logStr INFO $ "Done! Genetic Searchtook (" ++ showTime t ++ ") in CPU Time"

-- | Helper to safe all given patches to the corresponding files.
-- Files will start as fix1.patch in the given base-folder.
-- The files are in reverse order to have a nicer recursion - patch 1 is the last one found.
savePatchesToFiles :: 
  [String]  -- ^ The patches, represented as pretty-printed strings
  -> String -- ^ The folder in which to safe the patches
  -> IO ()
savePatchesToFiles [] _ = return ()
savePatchesToFiles patches@(p:ps) dir = do
    let n = length patches
    saveToFile p (dir ++ "/fix" ++ (show n) ++ ".patch")
    savePatchesToFiles ps dir

-- | Safes the given String to a file. 
-- Throws an Error in case the file already existet 
-- (this is a bit chicken, but I want this app to be safe so no wildcard overwriting of stuff).
-- To be repeatably usable, we just add the current timestamp to the output directory upstream, 
-- that is we make a folder output-yy-mm-dd-hh-mm and start writing patch1 patch2 ...
saveToFile :: 
  String -- ^ The Content of the file to be created 
  -> String -- ^ The Path to the file to be created, including the file name (e.g. "./tmp/fileA.txt")
  -> IO ()
saveToFile content path = do 
  fileExists <- doesFileExist path
  if fileExists
  then error "File already existed - aborting creation of patch"
  else do 
    -- handle <- openFile path ReadWriteMode
    writeFile path content
    --hClose handle
    return ()

-- | Returns the current time as yyyy-mm-dd-HH-MM 
formattedTime :: IO String
formattedTime = do 
  time <- getCurrentTime
  let format = "%Y-%m-%d-%HH-%MM"
  -- TODO: Get the users TimeZone from IO or from Config ? 
  let locale = defaultTimeLocale
  return (formatTime locale format time)
