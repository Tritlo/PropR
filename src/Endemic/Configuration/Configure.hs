{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Endemic.Configuration.Configure
  ( lOGCONFIG,
    getConfiguration,
    getConfiguration',
    setGlobalFlags,
    CLIOptions (..),
    newSeed,
    setSeedGenSeed,
  )
where

import Data.Aeson (eitherDecodeFileStrict', eitherDecodeStrict')
import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as BS
import Data.Default
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import qualified Data.Map as Map
import Data.Tuple (swap)
import Endemic.Configuration.Materializeable
import Endemic.Configuration.Types
import Endemic.Types
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
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
    case seed of
      Just i -> do
        putStrLn "Setting sm gen to"
        print i
        setSeedGenSeed i
        putStrLn "First res"
        newSeed >>= print
      _ -> return ()
    writeIORef lOGCONFIG lc

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

getConfiguration' :: CLIOptions -> IO Configuration
getConfiguration' opts@CLIOptions {optConfig = Nothing} = do
  seed <- newSeed
  addCliArguments opts (materialize (Just conjure)) {randomSeed = Just seed}
getConfiguration' opts@CLIOptions {optConfig = Just fp} =
  readConf fp >>= addCliArguments opts . materialize . Just

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
  let umLogConf =
        UmLogConf
          { umLogLoc = optLogLoc,
            umLogLevel = optLogLevel,
            umLogTimestamp = optLogTimestamp,
            umLogFile = optLogFile
          }

  conf'@Conf {..} <-
    case optOverride of
      Nothing -> return conf
      Just c -> override conf . Just <$> readConf c
  return $
    conf'
      { logConfig = override logConfig (Just umLogConf),
        randomSeed = mbOverride randomSeed optRandomSeed
      }
