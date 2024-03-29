{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (join, unless, when)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Default (def)
import Data.IORef (IORef, atomicModifyIORef', readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.LocalTime (utc)
import Data.Version (showVersion)
import GHC (HsExpr (HsLet), NoExtField (..))
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import qualified Paths_PropR as PE (version)
import PropR
import PropR.Check (checkImports)
import PropR.Diff
import PropR.Eval
import PropR.Packages (repairPackage)
import PropR.Repair (detranslate, translate)
import PropR.Search.Genetic (geneticSearchPlusPostprocessing, runGenMonad)
import PropR.Traversals (replaceExpr)
import PropR.Types
import PropR.Util
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.IO
import System.Random

data OptPicked
  = Repair {opts :: CLIOptions, clTarget :: String}
  | DumpConfig {opts :: CLIOptions, dcFlagSet :: Bool}
  | ShowVersion {opts :: CLIOptions, vFlagSet :: Bool}

optParser :: ParserInfo OptPicked
optParser = info (pickOpt <**> helper) modinfo
  where
    locParse =
      optional $
        flag'
          True
          ( long "log-loc"
              <> help "Add location to log messages"
          )
          <|> flag'
            False
            ( long "no-log-loc"
                <> help "Remove locations from log messages"
                <> internal
            )
    tsParse =
      optional $
        flag'
          True
          ( long "log-timestamp"
              <> help "Add timestamps to log messages"
              <> internal
          )
          <|> flag'
            False
            ( long "no-log-timestamp"
                <> help "Remove timestamps from log messages"
            )
    lvlParse =
      optional $
        option
          auto
          ( long "log-level"
              <> metavar "LOGLEVEL"
              <> showDefault
              <> help "The logging level to use"
          )
    logFileParse =
      optional $
        strOption
          ( long "log-file"
              <> metavar "FILE"
              <> help "Append logs to FILE"
          )
    randSeed =
      optional $
        option
          auto
          ( long "seed"
              <> metavar "INT"
              <> help
                ( "The random seed to use. "
                    ++ "Generated at runtime if not provided."
                )
          )
    confParse =
      optional $
        strOption
          ( long "config"
              <> metavar "CONFIG"
              <> help
                ( "The configuration to use. "
                    ++ fileJsonDesc
                    ++ ". Use --dump-config"
                    ++ " to see the current configuration"
                )
          )
    fileJsonDesc =
      "CONF can either be a path to a JSON file, "
        ++ "or the JSON can be specified directly"
    overrideParse =
      optional $
        strOption
          ( long "override"
              <> metavar "CONFIG"
              <> help
                ( "Override the configuration with the given CONFIG. "
                    ++ fileJsonDesc
                )
          )
    targetParse = strArgument (metavar "TARGET")
    cliOpts =
      CLIOptions <$> locParse
        <*> tsParse
        <*> lvlParse
        <*> logFileParse
        <*> randSeed
        <*> confParse
        <*> overrideParse
    pickOpt =
      ( (flip Repair <$> targetParse)
          <|> (flip DumpConfig <$> dumpConfig)
          <|> (flip ShowVersion <$> version)
      )
        <*> cliOpts
    modinfo =
      briefDesc
        <> progDesc "Repair TARGET using the PropR TLSR method"
        <> header ("PropR " ++ showVersion PE.version ++ " - Property-based Program Repair for Haskell")
    dumpConfig =
      flag'
        True
        ( long "dump-config"
            <> help
              ( "Dump the current configuration"
                  ++ " with all overrides and flags applied"
              )
        )
    version =
      flag'
        True
        ( long "version"
            <> help
              "Print version information"
        )

repairModule :: Configuration -> FilePath -> IO [String]
repairModule conf@Conf {..} target = do
  -- Set the global flags
  describeProblem conf target
    >>= \case
      Nothing ->
        logStr INFO "All props are passing, nothing to repair." >> return []
      Just desc@ProbDesc {..} -> do
        let p@EProb {..} = progProblem
        logStr VERBOSE $ "PROGRAM TO REPAIR: "
        logStr VERBOSE $ showUnsafe e_prog

        logStr INFO "REPAIRING..."
        fixes <- runRepair searchAlgorithm desc
        logStr TRACE "DONE! Fixes:"
        mapM_ (logOut DEBUG) fixes
        logStr TRACE "After applying:"
        mapM_ (logOut DEBUG . applyFixToEProg e_prog) fixes
        return $ fixesToDiffs desc fixes

main :: IO ()
main = do
  optPicked <- execParser optParser
  let clOpts@CLIOptions {..} = opts optPicked

  conf@Conf {..} <- getConfiguration clOpts
  case optPicked of
    DumpConfig _ _ -> BS.putStrLn (encode conf)
    ShowVersion _ _ -> putStrLn $ "PropR version " ++ showVersion PE.version
    Repair _ target -> do
      setGlobalFlags conf
      isDir <- doesDirectoryExist target
      (t, diffs) <- time $ (if isDir then repairPackage else repairModule) conf target

      reportStats' TIMINGS
      logStr INFO $ "Done! Repair search took (" ++ showTime t ++ ")"
      mapM_ (putStrLn . colorizeDiff) diffs

      when (savePatches outputConfig) $ do
        -- Here we write the found solutions to respective files, we just number them 1 .. n
        formTime <- formattedTime outputConfig
        let dir' = directory outputConfig ++ formTime
            oc = outputConfig {directory = dir'}
        dirExists <- doesDirectoryExist dir'
        unless dirExists $ createDirectory dir'
        savePatchesToFiles oc diffs
