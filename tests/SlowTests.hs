{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Default
import Data.IORef (readIORef, writeIORef)
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
import GhcPlugins (noLoc)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ tastyFixTests,
      randTests,
      properGenTests,
      genTests,
      exhaustiveTests
    ]

-- | Chosen fairly by Random.org
tESTSEED :: Int
tESTSEED = 703_039_772

tESTGENCONF :: GeneticConfiguration
tESTGENCONF = def {crossoverRate = 0.95, mutationRate = 0.05, dropRate = 0.05}

-- | For debugging tests
setLogLevel :: LogLevel -> IO ()
setLogLevel lvl = do
  lc <- readIORef lOGCONFIG
  writeIORef lOGCONFIG lc {logLevel = lvl}

mkRepairTest :: (ProblemDescription -> IO (Set EFix)) -> Integer -> TestName -> FilePath -> [String] -> TestTree
mkRepairTest how timeout tag file expected =
  localOption (mkTimeout timeout) $
    testCase tag $ do
      setSeedGenSeed (tESTSEED + 5)
      desc <- describeProblem def file
      fixes <- how desc
      fixesToDiffs desc fixes @?= expected

mkGenConfTest :: Integer -> TestName -> FilePath -> [String] -> TestTree
mkGenConfTest = mkRepairTest (\desc -> runGenMonad tESTGENCONF desc tESTSEED geneticSearchPlusPostprocessing)

mkSearchTest :: SearchAlgorithm -> Integer -> TestName -> FilePath -> [String] -> TestTree
mkSearchTest search_conf = mkRepairTest (runRepair search_conf)

mkPseudoGenTest :: Integer -> TestName -> FilePath -> [String] -> TestTree
mkPseudoGenTest = mkRepairTest (pseudoGeneticRepair def)

tastyFixTests :: TestTree
tastyFixTests =
  testGroup
    "Tasty fix tests"
    [ mkGenConfTest 30_000_000 "Repair TastyFix" "tests/cases/TastyFix.hs" $
        map
          unlines
          [ [ "diff --git a/tests/cases/TastyFix.hs b/tests/cases/TastyFix.hs",
              "--- a/tests/cases/TastyFix.hs",
              "+++ b/tests/cases/TastyFix.hs",
              "@@ -7,1 +7,1 @@ x = 2",
              "-x = 2",
              "+x = 3"
            ]
          ],
      mkGenConfTest 30_000_000 "Repair TastyMix" "tests/cases/TastyMix.hs" $
        map
          unlines
          [ [ "diff --git a/tests/cases/TastyMix.hs b/tests/cases/TastyMix.hs",
              "--- a/tests/cases/TastyMix.hs",
              "+++ b/tests/cases/TastyMix.hs",
              "@@ -7,1 +7,1 @@ x = 2",
              "-x = 2",
              "+x = 3"
            ]
          ],
      mkGenConfTest 180_000_000 "Repair TastyTwoFix" "tests/cases/TastyTwoFix.hs" $
        map
          unlines
          [ [ "diff --git a/tests/cases/TastyTwoFix.hs b/tests/cases/TastyTwoFix.hs",
              "--- a/tests/cases/TastyTwoFix.hs",
              "+++ b/tests/cases/TastyTwoFix.hs",
              "@@ -7,1 +7,1 @@ wrong_pair = (1, 2)",
              "-wrong_pair = (1, 2)",
              "+wrong_pair = (3, 4)"
            ]
          ]
    ]

randTests :: TestTree
randTests =
  testGroup
    "Random search tests"
    [ let conf = Random def {randStopOnResults = True, randIgnoreFailing = True}
       in mkSearchTest conf 60_000_000 "Repair TastyFix" "tests/cases/TastyFix.hs" $
            map
              unlines
              [ [ "diff --git a/tests/cases/TastyFix.hs b/tests/cases/TastyFix.hs",
                  "--- a/tests/cases/TastyFix.hs",
                  "+++ b/tests/cases/TastyFix.hs",
                  "@@ -7,1 +7,1 @@ x = 2",
                  "-x = 2",
                  "+x = 3"
                ]
              ]
    ]

exhaustiveTests :: TestTree
exhaustiveTests =
  testGroup
    "Exhaustive search tests"
    [ let conf = Exhaustive def {exhStopOnResults = True}
       in mkSearchTest conf 60_000_000 "Repair TastyFix" "tests/cases/TastyFix.hs" $
            map
              unlines
              [ [ "diff --git a/tests/cases/TastyFix.hs b/tests/cases/TastyFix.hs",
                  "--- a/tests/cases/TastyFix.hs",
                  "+++ b/tests/cases/TastyFix.hs",
                  "@@ -7,1 +7,1 @@ x = 2",
                  "-x = 2",
                  "+x = 3"
                ]
              ],
      let conf = Exhaustive def {exhStopOnResults = True}
       in mkSearchTest conf 180_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs" $
            map
              unlines
              [ [ "diff --git a/tests/cases/TwoFixes.hs b/tests/cases/TwoFixes.hs",
                  "--- a/tests/cases/TwoFixes.hs",
                  "+++ b/tests/cases/TwoFixes.hs",
                  "@@ -12,1 +12,1 @@ brokenPair = (1, 2)",
                  "-brokenPair = (1, 2)",
                  "+brokenPair = (3, 4)"
                ]
              ]
    ]

properGenTests :: TestTree
properGenTests =
  testGroup
    "Genetic search tests"
    [ mkGenConfTest 60_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs" $
        map
          unlines
          [ [ "diff --git a/tests/cases/ThreeFixes.hs b/tests/cases/ThreeFixes.hs",
              "--- a/tests/cases/ThreeFixes.hs",
              "+++ b/tests/cases/ThreeFixes.hs",
              "@@ -20,1 +20,1 @@ brokenPair = (1, 2, 3)",
              "-brokenPair = (1, 2, 3)",
              "+brokenPair = (3, 4, 5)"
            ]
          ],
      mkGenConfTest 15_000_000 "Repair UsesDependency" "tests/cases/UsesDependency.hs" $
        map
          unlines
          [ [ "diff --git a/tests/cases/UsesDependency.hs b/tests/cases/UsesDependency.hs",
              "--- a/tests/cases/UsesDependency.hs",
              "+++ b/tests/cases/UsesDependency.hs",
              "@@ -9,1 +9,1 @@ result = dependency + 3",
              "-result = dependency + 3",
              "+result = dependency + one"
            ]
          ]
    ]

genTests :: TestTree
genTests =
  testGroup
    "PseudoGenetic search tests"
    [ mkPseudoGenTest 120_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs" $
        map
          unlines
          [ [ "diff --git a/tests/cases/TwoFixes.hs b/tests/cases/TwoFixes.hs",
              "--- a/tests/cases/TwoFixes.hs",
              "+++ b/tests/cases/TwoFixes.hs",
              "@@ -12,1 +12,1 @@ brokenPair = (1, 2)",
              "-brokenPair = (1, 2)",
              "+brokenPair = (3, 4)"
            ]
          ],
      mkPseudoGenTest 75_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs" $
        map
          unlines
          [ [ "diff --git a/tests/cases/ThreeFixes.hs b/tests/cases/ThreeFixes.hs",
              "--- a/tests/cases/ThreeFixes.hs",
              "+++ b/tests/cases/ThreeFixes.hs",
              "@@ -20,1 +20,1 @@ brokenPair = (1, 2, 3)",
              "-brokenPair = (1, 2, 3)",
              "+brokenPair = (3, 4, 5)"
            ]
          ],
      mkPseudoGenTest 90_000_000 "Repair FourFixes" "tests/cases/FourFixes.hs" $
        map
          unlines
          [ [ "diff --git a/tests/cases/FourFixes.hs b/tests/cases/FourFixes.hs",
              "--- a/tests/cases/FourFixes.hs",
              "+++ b/tests/cases/FourFixes.hs",
              "@@ -24,1 +24,1 @@ brokenPair = (1, 2, 3, 4)",
              "-brokenPair = (1, 2, 3, 4)",
              "+brokenPair = (3, 4, 5, 6)"
            ]
          ]
    ]

main :: IO ()
main = defaultMain tests
