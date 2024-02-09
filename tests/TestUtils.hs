{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module TestUtils where

import Control.Monad (when)
import Data.Default
import Data.Functor (($>))
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import PropR
import PropR (describeProblem, getExprFitCands)
import PropR.Configuration
import PropR.Diff (applyFixes, fixesToDiffs, getFixBinds, ppDiff)
import PropR.Eval
import PropR.Search
import PropR.Search.Exhaustive
import PropR.Search.PseudoGenetic (pseudoGeneticRepair)
import PropR.Traversals
import PropR.Types
import PropR.Util
import GHC (tm_parsed_module, reLoc)
import System.Directory
import System.IO
import Test.Tasty
import Test.Tasty.HUnit

-- | Chosen fairly by Random.org
tESTSEED :: Int
tESTSEED = 703_039_772

tESTGENCONF :: GeneticConfiguration
tESTGENCONF = def {crossoverRate = 0.4, mutationRate = 0.1, dropRate = 0.25, iterations = 1_000}

tESTCONF :: Configuration
-- We want to try all the fancy features in the tests:
tESTCONF = def {compileConfig = def {allowFunctionFits = True,
                                     extendDefaults = True
                                     }}

data TestConf = TestConf
  { mb_expected :: Maybe [String],
    indices :: Maybe [Int],
    allowMix :: Bool,
    reportTestStats :: Bool,
    acceptNew :: Bool
  }
  deriving (Show, Eq)

instance Default TestConf where
  def =
    TestConf
      { mb_expected = Nothing,
        indices = Nothing,
        allowMix = False,
        reportTestStats = False,
        acceptNew = False
      }

mkSimpleModuleTest :: Integer -> TestName -> FilePath -> Maybe String -> TestTree
mkSimpleModuleTest timeout tag toFix repair_target =
  localOption (mkTimeout timeout) $
    testCase tag $
      withLogLevel GHCERR $ do
        expected <- readExpected toFix
        setSeedGenSeed tESTSEED
        (cc', mod, mb_prob) <- moduleToProb (compileConfig tESTCONF) toFix repair_target
        case mb_prob of
          Nothing -> [] @?= expected
          Just ExProb {..} -> error "not supported yet!"
          Just tp@EProb {..} -> do
            fixes <- repair cc' tp
            let fixProgs = map (eProgToEProgFix . applyFixToEProg e_prog) fixes
                diffs =
                  map
                    ( concatMap ppDiff
                        . map (\(a,b) -> (reLoc a, reLoc b))
                        . snd
                        . applyFixes (tm_parsed_module mod)
                        . getFixBinds
                        . head
                    )
                    fixProgs
                check = sort diffs == sort expected
                msg =
                  unlines
                    [ "Fix mismatch!",
                      "Expected:",
                      unlines expected,
                      "But got:",
                      unlines diffs,
                      "Actual fixes were:",
                      unlines (map showUnsafe fixes)
                    ]
            when (not check && acceptNew def) $ writeExpected toFix (unlines diffs)
            assertBool msg check

mkRepairTest ::
  Configuration ->
  (ProblemDescription -> IO (Set EFix)) ->
  Integer ->
  TestName ->
  FilePath ->
  TestTree
mkRepairTest conf how timeout tag file = mkRepairTest' conf how timeout tag file def

mkRepairTest' ::
  -- | The configuration to use
  Configuration ->
  -- | How to do the  repair
  (ProblemDescription -> IO (Set EFix)) ->
  -- | The timeout for the test
  Integer ->
  -- | The tag for the tests
  TestName ->
  -- | The module to repair
  FilePath ->
  TestConf ->
  TestTree
mkRepairTest' conf how timeout tag file TestConf {..} =
  localOption (mkTimeout timeout) $
    testCase tag $ do
      expected' <- case mb_expected of
        Just x -> return x
        Nothing -> readExpected file
      let expected = case indices of
            Just inds -> map (expected' !!) $ take (length expected') inds
            _ -> expected'

      setSeedGenSeed (tESTSEED + 5)
      resetStats
      desc <- collectStats (describeProblem conf file)
      ( \case
          Just desc -> do
            fixes <- collectStats $ how desc

            diffs <- collectStats $ return (fixesToDiffs desc fixes)
            let check' = sort diffs == sort expected || (null expected && all Map.null fixes)
                check = check' || (allowMix && not (Set.null (Set.fromList diffs `Set.intersection` Set.fromList expected)))
                msg =
                  unlines
                    [ "Fix mismatch!",
                      "Expected:",
                      unlines expected,
                      "But got:",
                      unlines diffs,
                      "Actual fixes were:",
                      unlines (map showUnsafe $ Set.toList fixes),
                      "Number of fixes:",
                      show (Set.size fixes),
                      "Number of expected:",
                      show (length expected)
                    ]
            when reportTestStats (putStrLn "" >> putStrLn "Stats:" >> getStats >>= mapM_ putStrLn)
            when (not check && acceptNew) $ writeExpected file (unlines diffs)
            assertBool msg check
          Nothing -> do
            when reportTestStats (putStrLn "" >> putStrLn "Stats:" >> getStats >>= mapM_ putStrLn)
            [] @?= expected
        )
        desc

readExpected :: FilePath -> IO [String]
readExpected fp = do
  ls <- lines <$> readFile fp
  let ex' =
        takeWhile (/= "---- END EXPECTED ----") $
          drop 1 $
            dropWhile (/= "---- EXPECTED ----") ls
      ex'' = map (drop 3) ex'
      diffs = split "" ex''
  return $ map unlines diffs

writeExpected :: FilePath -> String -> IO ()
writeExpected fp to_write = do
  putStrLn "Accepting new output!"
  ls <- lines <$> readFile fp
  print (length ls)
  let new =
        ("---- EXPECTED ----" : map addDashes (init $ lines to_write))
          ++ ["---- END EXPECTED ----"]
      addDashes [] = "--"
      addDashes xs = "-- " ++ xs
  let beg = takeWhile (/= "---- EXPECTED ----") ls
      newo = unlines $ beg ++ new
  writeFile (fp ++ ".tmp") newo
  renameFile (fp ++ ".tmp") fp
