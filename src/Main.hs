{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
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
import Endemic.Search.Genetic (ProblemDescription (..), describeProblem, runGenMonad, geneticSearchPlusPostprocessing)
import Endemic.Traversals (replaceExpr)
import Endemic.Types
import Endemic.Util
import GHC (HsExpr (HsLet), NoExtField (..))
import GhcPlugins (noLoc)
import System.Directory (createDirectory)
import System.Environment (getArgs)
import System.IO
import System.Random

import Options.Applicative

optParser :: ParserInfo CLIOptions
optParser = info (cliOpts <**> helper) modinfo
  where locParse = optional $ (flag' True ( long "log-loc"
                                    <> help "Add location to log messages"
                                    ))
                         <|> (flag' False ( long "no-log-loc"
                                    <> help "Remove locations from log messages"
                                    <> internal ))
        tsParse = optional $ (flag' True ( long "log-timestamp"
                                    <> help "Add timestamps to log messages"
                                    <> internal ))
                         <|> (flag' False ( long "no-log-timestamp"
                                    <> help "Remove timestamps from log messages"))
        lvlParse = optional $ option auto ( long "log-level"
                                         <> metavar "LOGLEVEL"
                                         <> value WARN
                                         <> showDefault
                                         <> help "The logging level to use" )
        logFileParse = optional $ strOption  (  long "log-file"
                                             <> metavar "FILE"
                                             <> help "Append logs to FILE")
        randSeed = optional $ option auto ( long "seed"
                                          <> metavar "INT"
                                          <> help ("The random seed to use. "
                                                 ++ "Generated at runtime if not provided."))
        confParse = optional $ strOption ( long "config"
                                         <> metavar "CONFIG"
                                        <> help ("The configuration to use. "
                                                ++ fileJsonDesc))
        fileJsonDesc = "CONF can either be a path to a JSON file, "
                    ++ "or the JSON can be specified directly"
        overrideParse = optional $
             strOption ( long "override"
                      <> metavar "CONFIG"
                      <> help ( "Override the configuration with the given CONFIG. "
                             ++ fileJsonDesc) )
        targetsParse = strArgument (metavar "TARGET")
        cliOpts = CLIOptions <$> locParse <*>
                                 tsParse <*>
                                 lvlParse <*>
                                 logFileParse <*>
                                 randSeed <*>
                                 confParse <*>
                                 overrideParse <*>
                                 targetsParse
        modinfo = (briefDesc
            <> progDesc "Repair TARGET using the endemic genetic method"
            <> header "endemic - Genetic program repair for Haskell")



main :: IO ()
main = do
  opts@CLIOptions{..} <- execParser optParser
  conf@Conf {..} <- getConfiguration opts
  -- Set the global flags
  setGlobalFlags conf

  (_, modul, probs) <- moduleToProb compileConfig optTarget Nothing
  let (tp@EProb {..} : _) = if null probs then error "NO TARGET FOUND!" else probs
      RProb {..} = detranslate tp
  logStr INFO $ "TARGET:"
  logStr INFO $ ("  `" ++ r_target ++ "` in " ++ optTarget)
  logStr VERBOSE $ "CONFIG:"
  logStr VERBOSE $ show conf
  logStr VERBOSE $ "SCOPE:"
  mapM_ (logStr VERBOSE . ("  " ++)) (importStmts compileConfig)
  logStr INFO $ "TARGET TYPE:"
  logStr INFO $ "  " ++ r_ty
  logStr INFO $ "MUST SATISFY:"
  mapM_ (logStr INFO . ("  " ++)) r_props
  logStr VERBOSE $ "IN CONTEXT:"
  mapM_ (logStr VERBOSE . ("  " ++)) r_ctxt
  logStr VERBOSE $ "PROGRAM TO REPAIR: "
  logStr VERBOSE $ showUnsafe e_prog

  logStr INFO "REPAIRING..."
  desc <- describeProblem conf optTarget
  seed <- case optRandomSeed of
            Just s -> return s
            _ -> randomIO
  (t, fixes) <- time $ runGenMonad def desc seed geneticSearchPlusPostprocessing
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
  reportStats' VERBOSE
  logStr INFO $ "Done! Genetic search took (" ++ showTime t ++ ") in CPU Time"
