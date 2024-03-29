{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module PropR.Configuration.Configure
  ( lOGCONFIG,
    getConfiguration,
    getConfiguration',
    setGlobalFlags,
    CLIOptions (..),
    newSeed,
    setSeedGenSeed,
    withFrozenSeedGen,
  )
where

import Control.Monad
import Data.Aeson (eitherDecodeFileStrict', eitherDecodeStrict')
import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as BS
import Data.Default
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import GHC.Generics (Generic)
import PropR.Configuration.Materializeable
import PropR.Configuration.Types
import PropR.Types
import System.Directory (doesFileExist)
import System.FilePath (takeExtension)
import System.IO.Unsafe (unsafePerformIO)
import System.Random.SplitMix

-- | Global variable to configure logging
{-# NOINLINE lOGCONFIG #-}
lOGCONFIG :: IORef LogConfig
lOGCONFIG = unsafePerformIO $ newIORef def

-- | Global variable to keep track of the current quickcheck seed.
-- TODO: We should define a RepMonad (including all the arguments)
-- and maintain the gen there, like in the GenMonad.
{-# NOINLINE sEEDGEN #-}
sEEDGEN :: IORef SMGen
sEEDGEN = unsafePerformIO $ initSMGen >>= newIORef

newSeed :: IO Int
newSeed = atomicModifyIORef' sEEDGEN (swap . nextInt)

-- | Freeze the seed gen for the current action, meaning we restore it back
-- to it's original value after it's been used. Useful for checking e.g. if
-- a change is due to a different seed or an actual change.
withFrozenSeedGen :: IO a -> IO a
withFrozenSeedGen a = do
  s <- readIORef sEEDGEN
  r <- a
  writeIORef sEEDGEN s
  return r

setSeedGenSeed :: Int -> IO ()
setSeedGenSeed = writeIORef sEEDGEN . mkSMGen . fromIntegral

-- | Set global flags sets the global flags to the values specified in
-- configuration, i.e. the `lOGLOC`, `lOGLEVEL` and `dEBUG`, and the
-- IO random generator from the random seed.
setGlobalFlags :: Configuration -> IO ()
setGlobalFlags
  Conf
    { logConfig = lc,
      randomSeed = seed
    } = do
    forM_ seed setSeedGenSeed
    writeIORef lOGCONFIG lc

readConf :: String -> IO (Unmaterialized Configuration)
readConf fp = do
  fileExists <- doesFileExist fp
  res <-
    if fileExists
      then eitherDecodeFileStrict' fp
      else return $ eitherDecodeStrict' (BS.pack fp)
  case res of
    Left err | ('/' `elem` fp) || ".json" == takeExtension fp -> do
      if fileExists
        then error err
        else error $ "Could not find " ++ fp ++ "!"
    Left err | '{' `elem` fp -> do
      putStrLn $ "Could not decode " ++ fp ++ " as JSON"
      putStrLn "Decode failed with:"
      error err
    Left err -> error err
    Right um_conf -> return um_conf

getConfiguration' :: CLIOptions -> IO Configuration
getConfiguration' opts@CLIOptions {optConfig = Nothing} = do
  seed <- newSeed
  addCliArguments opts (materialize (Just conjure)) {randomSeed = Just seed}
getConfiguration' opts@CLIOptions {optConfig = Just fp} = do
  conf <- readConf fp >>= addCliArguments opts . materialize . Just
  case randomSeed conf of
    Just seed -> return conf
    Nothing -> do
      seed <- newSeed
      return conf {randomSeed = Just seed}

-- | Parses the given configuration or reads it from file (if it's a file), and
-- sets the internal global flags from the configuration. Use getConfiguration'
-- if you just want the configuration.
-- Returns a default configuration if none is given.
getConfiguration :: CLIOptions -> IO Configuration
getConfiguration opts =
  do
    c <- getConfiguration' opts
    setGlobalFlags c
    return c

data CLIOptions = CLIOptions
  { optLogLoc :: Maybe Bool,
    optLogTimestamp :: Maybe Bool,
    optLogLevel :: Maybe LogLevel,
    optLogFile :: Maybe String,
    optRandomSeed :: Maybe Int,
    optConfig :: Maybe String,
    optOverride :: Maybe String
  }
  deriving (Eq, Show, Generic)

instance Default CLIOptions where
  def = CLIOptions n n n n n n n
    where
      n = Nothing

addCliArguments :: CLIOptions -> Configuration -> IO Configuration
addCliArguments CLIOptions {..} conf = do
  conf'@Conf {logConfig = logConfig@LogConf {..}, ..} <-
    case optOverride of
      Nothing -> return conf
      Just c -> override conf . Just <$> readConf c
  let logConf' =
        LogConf
          { logLoc = fromMaybe logLoc optLogLoc,
            logLevel = fromMaybe logLevel optLogLevel,
            logTimestamp = fromMaybe logTimestamp optLogTimestamp,
            logFile = mbOverride logFile optLogFile
          }
  return $
    conf'
      { logConfig = logConf',
        randomSeed = mbOverride randomSeed optRandomSeed
      }
