{-# LANGUAGE RecordWildCards #-}

module Endemic.Packages where

import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parsec
import Distribution.Pretty (prettyShow)
import Distribution.Simple
import Distribution.Simple.Build
import Distribution.Simple.Configure
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Types.GenericPackageDescription
import Distribution.Types.LocalBuildInfo
import Distribution.Verbosity
import Endemic
import System.Directory

-- | Finds the cabal file in the given file, errors if there's no such file or
-- multiple files.
findCabalFile :: IO FilePath
findCabalFile = do
  mb_cabal_file <- filter isCabal <$> listDirectory "."
  case mb_cabal_file of
    (_ : _ : _) -> error $ "Multiple cabal files found!"
    [] -> error $ "No cabal file found!"
    [cabal_file] -> makeAbsolute cabal_file
  where
    rev_cabal = reverse ".cabal"
    isCabal = (rev_cabal ==) . take (length rev_cabal) . reverse

repairPackage :: Configuration -> FilePath -> IO [String]
repairPackage conf@Conf {..} target_dir = withCurrentDirectory target_dir $ do
  cabal_file <- findCabalFile
  print cabal_file
  g_desc <- readGenericPackageDescription silent cabal_file
  lbi_res <- newIORef Nothing
  let hooks =
        simpleUserHooks
          { postBuild = \_ _ _ lbi ->
              writeIORef lbi_res (Just lbi)
          }
  defaultMainWithHooksNoReadArgs hooks g_desc ["configure"]
  defaultMainWithHooksNoReadArgs hooks g_desc ["build"]
  Just lbi <- readIORef lbi_res
  print lbi
  return []
