{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
module Endemic.Configuration.Configure
    (
      lOGCONFIG,
      getConfiguration,
      setGlobalFlags,
      CLIOptions(..),
    ) where

import Data.Bifunctor (second)
import System.Directory (doesFileExist)
import System.IO.Unsafe ( unsafePerformIO )
import System.Random (randomIO)
import Data.IORef ( IORef, newIORef, writeIORef )
import Endemic.Configuration.Types
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BS
import Data.Default (def, Default)
import Data.Aeson ( eitherDecodeFileStrict', eitherDecodeStrict' )
import Endemic.Types
import GHC.Generics (Generic)

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
getConfiguration :: CLIOptions -> IO Configuration
getConfiguration opts@CLIOptions{optConfig=Nothing} = do
  seed <- randomIO
  addCliArguments opts (materialize (Just conjure)) {randomSeed = Just seed}
getConfiguration opts@CLIOptions{optConfig=Just fp} =
  readConf fp >>= addCliArguments opts . materialize . Just

data CLIOptions = CLIOptions {
   optLogLoc :: Maybe Bool,
   optLogTimestamp :: Maybe Bool,
   optLogLevel :: Maybe LogLevel,
   optLogFile :: Maybe String,
   optRandomSeed :: Maybe Int,
   optConfig :: Maybe String,
   optOverride :: Maybe String
  } deriving (Eq, Show, Generic)


addCliArguments :: CLIOptions -> Configuration -> IO Configuration
addCliArguments CLIOptions{..} conf = do

  let umLogConf = UmLogConf { umLogLoc = optLogLoc
                            , umLogLevel = optLogLevel
                            , umLogTimestamp = optLogTimestamp
                            , umLogFile = optLogFile }

  conf'@Conf {..} <-
    case optOverride of
      Nothing -> return conf
      Just c -> override conf . Just <$> readConf c
  return $
    conf'
      { logConfig = override logConfig (Just umLogConf)
      , randomSeed = mbOverride randomSeed optRandomSeed
      }
