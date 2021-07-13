{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Default (def)
import Data.IORef (IORef, atomicModifyIORef', readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time.LocalTime (utc)
import Endemic
import Endemic.Check (buildSuccessCheck, checkImports)
import Endemic.Diff
import Endemic.Eval
import Endemic.Repair (detranslate, translate)
import Endemic.Search.Genetic (ProblemDescription (..), describeProblem)
import Endemic.Search.Genetic.GenMonad (runGenMonad)
import Endemic.Search.Genetic.Search (geneticSearchPlusPostprocessing)
import Endemic.Traversals (replaceExpr)
import Endemic.Types
import Endemic.Util
import GHC (HsExpr (HsLet), NoExtField (..))
import GhcPlugins (noLoc)
import System.Directory (createDirectory)
import System.Environment (getArgs)
import System.IO

main :: IO ()
main = do
  args <- Map.fromList . map (break (== '=')) <$> getArgs
  conf@Conf {..} <- getConfiguration (tail <$> (args Map.!? "--config"))

  -- Set the global flags
  setGlobalFlags conf

  [toFix] <- filter (not . (==) "-" . take 1) <$> getArgs
  (_, modul, probs) <- moduleToProb compileConfig toFix Nothing
  let (tp@EProb {..} : _) = if null probs then error "NO TARGET FOUND!" else probs
      RProb {..} = detranslate tp
  putStrLn "TARGET:"
  putStrLn ("  `" ++ r_target ++ "` in " ++ toFix)
  putStrLn "CONFIG:"
  BS.putStrLn $ encode conf
  putStrLn "SCOPE:"
  mapM_ (putStrLn . ("  " ++)) (importStmts compileConfig)
  putStrLn "TARGET TYPE:"
  putStrLn $ "  " ++ r_ty
  putStrLn "MUST SATISFY:"
  mapM_ (putStrLn . ("  " ++)) r_props
  putStrLn "IN CONTEXT:"
  mapM_ (putStrLn . ("  " ++)) r_ctxt
  putStrLn "PROGRAM TO REPAIR: "
  putStrLn $ showUnsafe e_prog

  putStrLn "REPAIRING..."
  desc <- describeProblem conf toFix
  (t, fixes) <- time $ runGenMonad def desc 69420 geneticSearchPlusPostprocessing
  let newProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
      fbs = map getFixBinds newProgs
  -- Here we write the found solutions to respective files, we just number them 1 .. n
  formTime <- formattedTime outputConfig
  -- TODO: Add a Configurable prefix for the output directory, e.g. /tmp/
  let outputDirectory = "./output-patches-" ++ formTime
      oc = outputConfig {directory = outputDirectory}
  createDirectory outputDirectory
  let prettyPrinted = map (concatMap ppDiff . snd . applyFixes modul) fbs
  savePatchesToFiles oc prettyPrinted
  mapM_ (putStrLn . concatMap (colorizeDiff . ppDiff) . snd . applyFixes modul) fbs
  reportStats' INFO
  putStrLn $ "DONE! (" ++ showTime t ++ ")"
