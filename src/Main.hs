{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (unless, when)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Default (def)
import Data.IORef (IORef, atomicModifyIORef', readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.LocalTime (utc)
import Endemic
import Endemic.Check (buildSuccessCheck, checkImports)
import Endemic.Diff
import Endemic.Eval
import Endemic.Repair (detranslate, translate)
import Endemic.Search.Genetic (geneticSearchPlusPostprocessing, runGenMonad)
import Endemic.Traversals (replaceExpr)
import Endemic.Types
import Endemic.Util
import GHC (HsExpr (HsLet), NoExtField (..))
import GhcPlugins (noLoc)
import Options.Applicative
import Options.Applicative.Types (readerAsk)
import System.Directory (createDirectory, doesDirectoryExist)
import System.Environment (getArgs)
import System.IO
import System.Random

data OptPicked
  = Repair {opts :: CLIOptions, clTarget :: String}
  | DumpConfig {opts :: CLIOptions, dcFlagSet :: Bool}

optParser :: ParserInfo OptPicked
optParser = info (pickOpt <**> helper) modinfo
  where
    locParse =
      optional $
        ( flag'
            True
            ( long "log-loc"
                <> help "Add location to log messages"
            )
        )
          <|> ( flag'
                  False
                  ( long "no-log-loc"
                      <> help "Remove locations from log messages"
                      <> internal
                  )
              )
    tsParse =
      optional $
        ( flag'
            True
            ( long "log-timestamp"
                <> help "Add timestamps to log messages"
                <> internal
            )
        )
          <|> ( flag'
                  False
                  ( long "no-log-timestamp"
                      <> help "Remove timestamps from log messages"
                  )
              )
    lvlParse =
      optional $
        option
          auto
          ( long "log-level"
              <> metavar "LOGLEVEL"
              <> value WARN
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
    pickOpt = ((flip Repair <$> targetParse) <|> (flip DumpConfig <$> dumpConfig)) <*> cliOpts
    modinfo =
      ( briefDesc
          <> progDesc "Repair TARGET using the endemic genetic method"
          <> header "endemic - Genetic program repair for Haskell"
      )
    dumpConfig =
      flag'
        True
        ( long "dump-config"
            <> help
              ( "Dump the current configuration"
                  ++ " with all overrides and flags applied"
              )
        )

main :: IO ()
main = do
  optPicked <- execParser optParser
  let clOpts@CLIOptions {..} = opts optPicked

  conf@Conf {..} <- getConfiguration clOpts
  case optPicked of
    DumpConfig _ _ -> BS.putStrLn (encode conf)
    Repair _ target -> do
      -- Set the global flags
      setGlobalFlags conf

      (_, modul, probs) <- moduleToProb compileConfig target Nothing
      let (tp@EProb {..} : _) = if null probs then error "NO TARGET FOUND!" else probs
          RProb {..} = detranslate tp
      logStr INFO $ "TARGET:"
      logStr INFO $ ("  `" ++ r_target ++ "` in " ++ target)
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
      seed <- case optRandomSeed of
        Just s -> return s
        _ -> randomIO
      desc <- describeProblem conf target
      (t, fixes) <- time $
        case searchAlgorithm of
          Genetic gconf -> runGenMonad gconf desc seed geneticSearchPlusPostprocessing
          PseudoGenetic pgc -> pseudoGeneticRepair pgc desc
      let newProgs = map (`replaceExpr` progAtTy e_prog e_ty) $ Set.toList fixes
          fbs = map getFixBinds newProgs
          prettyPrinted = map (concatMap ppDiff . snd . applyFixes modul) fbs

      when (savePatches outputConfig) $ do
        -- Here we write the found solutions to respective files, we just number them 1 .. n
        formTime <- formattedTime outputConfig
        let dir' = directory outputConfig ++ formTime
            oc = outputConfig {directory = dir'}
        dirExists <- doesDirectoryExist dir'
        unless dirExists $ createDirectory dir'
        savePatchesToFiles oc prettyPrinted

      mapM_ (putStrLn . concatMap (colorizeDiff . ppDiff) . snd . applyFixes modul) fbs
      reportStats' VERBOSE
      logStr INFO $ "Done! Genetic search took (" ++ showTime t ++ ") in CPU Time"
