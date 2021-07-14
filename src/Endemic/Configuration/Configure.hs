{-# LANGUAGE RecordWildCards #-}
module Endemic.Configuration.Configure
    (
      lOGCONFIG,
      getConfiguration,
      setGlobalFlags
    ) where

import Data.Bifunctor (second)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO.Unsafe ( unsafePerformIO )
import System.Random (randomIO)
import Data.IORef ( IORef, newIORef, writeIORef )
import Endemic.Configuration.Types
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Data.Default (def)
import Data.Aeson ( eitherDecodeFileStrict', eitherDecodeStrict' )

-- | Global variable to configure logging
{-# NOINLINE lOGCONFIG #-}
lOGCONFIG :: IORef LogConfig
lOGCONFIG = unsafePerformIO $ newIORef def

-- | Set global flags sets the global flags to the values specified in
-- configuration, i.e. the `lOGLOC`, `lOGLEVEL` and `dEBUG`.
setGlobalFlags :: Configuration -> IO ()
setGlobalFlags Conf {logConfig = lc} = writeIORef lOGCONFIG lc

readConf :: String -> IO (Unmaterialized Configuration)
readConf fp = do
  fileExists <- doesFileExist fp
  res <-
    if fileExists
      then eitherDecodeFileStrict' fp
      else return $ eitherDecodeStrict' (BS.pack fp)
  case res of
    Left err -> error err
    Right um_conf -> return um_conf

-- | Parses the given configuration or reads it from file (if it's a file).
-- Retursn a default configuration if none is given.
getConfiguration :: Maybe String -> IO Configuration
getConfiguration Nothing = do
  let conf = materialize (Just conjure)
  seed <- randomIO
  let conf' = conf {randomSeed = Just seed}
  addCliArguments conf'
getConfiguration (Just fp) = do
  readConf fp >>= addCliArguments . materialize . Just

-- TODO: Implement using opt-parse
addCliArguments :: Configuration -> IO Configuration
addCliArguments conf = do
  loc <- elem "--log-loc" <$> getArgs
  no_loc <- elem "--no-log-loc" <$> getArgs

  time <- elem "--log-timestamp" <$> getArgs
  no_time <- elem "--no-log-timestamp" <$> getArgs

  args <- Map.fromList . map (second (drop 1) . break (== '=')) <$> getArgs
  let mb_lvl = read <$> (args Map.!? "--log-level")
      mb_file = args Map.!? "--log-file"
      mb_seed = args Map.!? "--seed"

  conf'@Conf {..} <-
    case args Map.!? "--override" of
      Nothing -> return conf
      Just c -> do
        nc <- readConf c
        return $ override conf (Just nc)
  return $
    conf'
      { logConfig =
          logConfig
            { logLoc =
                if loc || no_loc
                  then loc
                  else logLoc logConfig,
              logLevel = case mb_lvl of
                Just lvl -> lvl
                _ -> logLevel logConfig,
              logFile = case mb_file of
                Just fp -> Just fp
                _ -> logFile logConfig,
              logTimestamp =
                if time || no_time
                  then time
                  else logTimestamp logConfig
            },
        randomSeed = case mb_seed of
          Just s -> read <$> Just s
          _ -> randomSeed
      }