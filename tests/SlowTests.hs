{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Default
import Data.IORef (readIORef, writeIORef)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic (describeProblem, getExprFitCands)
import Endemic.Configuration
import Endemic.Diff (applyFixes, fixesToDiffs, getFixBinds, ppDiff)
import Endemic.Eval
import Endemic.Search
import Endemic.Search.Exhaustive
import Endemic.Search.PseudoGenetic (pseudoGeneticRepair)
import Endemic.Traversals
import Endemic.Types
import Endemic.Util
import GHC (HsExpr (HsLet), NoExtField (NoExtField))
import GhcPlugins (noLoc, ppr, showSDocUnsafe)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ specialTests,
      tastyFixTests,
      randTests,
      properGenTests,
      genTests,
      exhaustiveTests
    ]

-- | Chosen fairly by Random.org
tESTSEED :: Int
tESTSEED = 703_039_772

tESTGENCONF :: GeneticConfiguration
tESTGENCONF = def {crossoverRate = 0.4, mutationRate = 0.1, dropRate = 0.25, iterations = 1_000}

mkRepairTest ::
  Configuration ->
  (ProblemDescription -> IO (Set EFix)) ->
  Integer ->
  TestName ->
  FilePath ->
  TestTree
mkRepairTest conf how timeout tag file = mkRepairTest' conf how timeout tag file Nothing

mkRepairTest' ::
  Configuration ->
  (ProblemDescription -> IO (Set EFix)) ->
  Integer ->
  TestName ->
  FilePath ->
  Maybe [String] ->
  TestTree
mkRepairTest' conf how timeout tag file mb_expected =
  localOption (mkTimeout timeout) $
    testCase tag $ do
      expected <- case mb_expected of
        Just x -> return x
        Nothing -> readExpected file
      setSeedGenSeed (tESTSEED + 5)
      describeProblem conf file
        >>= \case
          Just desc -> do
            fixes <- how desc

            let diffs = fixesToDiffs desc fixes
                check = sort diffs == sort expected || (null expected && all Map.null fixes)
                msg =
                  unlines
                    [ "Fix mismatch!",
                      "Expected:",
                      unlines expected,
                      "But got:",
                      unlines diffs,
                      "Actual fixes were:",
                      unlines (map (showSDocUnsafe . ppr) $ Set.toList fixes),
                      "Number of fixes:",
                      show (Set.size fixes)
                    ]
            assertBool msg check
          Nothing -> [] @?= expected

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

runGenRepair :: ProblemDescription -> IO (Set EFix)
runGenRepair desc = runGenMonad tESTGENCONF desc tESTSEED geneticSearchPlusPostprocessing

mkGenConfTestEx :: Integer -> TestName -> FilePath -> TestTree
mkGenConfTestEx = mkRepairTest def runGenRepair

mkSearchTestEx :: SearchAlgorithm -> Integer -> TestName -> FilePath -> TestTree
mkSearchTestEx search_conf = mkRepairTest def (runRepair search_conf)

mkPseudoGenTestEx :: Integer -> TestName -> FilePath -> TestTree
mkPseudoGenTestEx = mkRepairTest def (pseudoGeneticRepair def)

tastyFixTests :: TestTree
tastyFixTests =
  testGroup
    "Tasty fix tests"
    [ mkGenConfTestEx 30_000_000 "Repair TastyFix" "tests/cases/TastyFix.hs",
      mkGenConfTestEx 30_000_000 "Repair TastyMix" "tests/cases/TastyMix.hs",
      mkGenConfTestEx 180_000_000 "Repair TastyTwoFix" "tests/cases/TastyTwoFix.hs"
    ]

randTests :: TestTree
randTests =
  testGroup
    "Random search tests"
    [ let conf = Random def {randStopOnResults = True, randIgnoreFailing = True}
       in mkSearchTestEx conf 60_000_000 "Repair TastyFix" "tests/cases/TastyFix.hs"
    ]

exhaustiveTests :: TestTree
exhaustiveTests =
  testGroup
    "Exhaustive search tests"
    [ let conf = Exhaustive def {exhStopOnResults = True}
       in mkSearchTestEx conf 60_000_000 "Repair TastyFix" "tests/cases/TastyFix.hs",
      let conf = Exhaustive def {exhStopOnResults = True}
       in mkSearchTestEx conf 180_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs"
    ]

properGenTests :: TestTree
properGenTests =
  testGroup
    "Genetic search tests"
    [ mkGenConfTestEx 180_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkGenConfTestEx 60_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs",
      mkGenConfTestEx 60_000_000 "Repair FourFixes" "tests/cases/FourFixes.hs"
    ]

genTests :: TestTree
genTests =
  testGroup
    "PseudoGenetic search tests"
    [ mkPseudoGenTestEx 120_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkPseudoGenTestEx 75_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs",
      mkPseudoGenTestEx 90_000_000 "Repair FourFixes" "tests/cases/FourFixes.hs"
    ]

-- These all test compilation corner cases
specialTests :: TestTree
specialTests =
  testGroup
    "Special"
    [ mkGenConfTestEx 15_000_000 "Repair UsesDependency" "tests/cases/UsesDependency.hs",
      mkGenConfTestEx 15_000_000 "Repair Operators" "tests/cases/Operators.hs",
      mkGenConfTestEx 15_000_000 "Repair Data" "tests/cases/Data.hs",
      mkGenConfTestEx 60_000_000 "Repair Multi" "tests/cases/Multi.hs",
      mkGenConfTestEx 5_000_000 "All props pass" "tests/cases/AllPropsPass.hs",
      mkGenConfTestEx 5_000_000 "No props" "tests/cases/NoProps.hs"
    ]

main :: IO ()
main = defaultMain tests
