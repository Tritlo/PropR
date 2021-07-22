{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Default
import Data.IORef (readIORef)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as Set
import Endemic (getExprFitCands)
import Endemic.Configuration
import Endemic.Diff (applyFixes, getFixBinds, ppDiff)
import Endemic.Eval
import Endemic.Search
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
    [tastyFixTests, randTests, properGenTests, genTests]

-- | Chosen fairly by Random.org
tESTSEED :: Int
tESTSEED = 703_039_772

tESTGENCONF :: GeneticConfiguration
tESTGENCONF = def {crossoverRate = 0.95, mutationRate = 0.05, dropRate = 0.05}

tastyFixTests :: TestTree
tastyFixTests =
  testGroup
    "Tasty fix tests"
    [ localOption (mkTimeout 30_000_000) $
        testCase "Repair TastyFix" $ do
          let toFix = "tests/cases/TastyFix.hs"
              repair_target = Nothing
              expected =
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

          setSeedGenSeed (tESTSEED + 5)
          (_, modul, [EProb {..}]) <-
            moduleToProb (def {packages = def packages ++ ["tasty", "tasty-hunit"]}) toFix repair_target
          desc <- describeProblem def toFix
          fixes <- runGenMonad tESTGENCONF desc tESTSEED geneticSearchPlusPostprocessing
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) $ Set.toList fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes modul . getFixBinds) fixProgs
          fixDiffs @?= expected,
      localOption (mkTimeout 30_000_000) $
        testCase "Repair TastyMix" $ do
          let toFix = "tests/cases/TastyMix.hs"
              repair_target = Nothing
              expected =
                map
                  unlines
                  [ [ "diff --git a/tests/cases/TastyMix.hs b/tests/cases/TastyMix.hs",
                      "--- a/tests/cases/TastyMix.hs",
                      "+++ b/tests/cases/TastyMix.hs",
                      "@@ -7,1 +7,1 @@ x = 2",
                      "-x = 2",
                      "+x = 3"
                    ]
                  ]

          setSeedGenSeed tESTSEED
          (_, modul, [EProb {..}]) <-
            moduleToProb (def {packages = def packages ++ ["tasty", "tasty-hunit"]}) toFix repair_target
          desc <- describeProblem def toFix
          fixes <- runGenMonad tESTGENCONF desc tESTSEED geneticSearchPlusPostprocessing
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) $ Set.toList fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes modul . getFixBinds) fixProgs
          fixDiffs @?= expected
    ]

randTests :: TestTree
randTests =
  testGroup
    "Random search tests"
    [ localOption (mkTimeout 60_000_000) $
        testCase "Repair TastyFix" $ do
          let toFix = "tests/cases/TastyFix.hs"
              expected =
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
              randConf = def {randStopOnResults = True, randIgnoreFailing = True}
          setSeedGenSeed (tESTSEED + 5)
          fixes <- describeProblem def toFix >>= runRepair (Random randConf)
          (_, modul, [EProb {..}]) <- moduleToProb def toFix Nothing
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) $ Set.toList fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes modul . getFixBinds) fixProgs
          fixDiffs @?= expected
    ]

properGenTests :: TestTree
properGenTests =
  testGroup
    "proper generation tests"
    [ localOption (mkTimeout 60_000_000) $
        testCase "Repair ThreeFixes" $ do
          let toFix = "tests/cases/ThreeFixes.hs"
              repair_target = Nothing
              expected =
                map
                  unlines
                  [ [ "diff --git a/tests/cases/ThreeFixes.hs b/tests/cases/ThreeFixes.hs",
                      "--- a/tests/cases/ThreeFixes.hs",
                      "+++ b/tests/cases/ThreeFixes.hs",
                      "@@ -20,1 +20,1 @@ brokenPair = (1, 2, 3)",
                      "-brokenPair = (1, 2, 3)",
                      "+brokenPair = (3, 4, 5)"
                    ]
                  ]
          setSeedGenSeed (tESTSEED + 5)
          (_, modul, [EProb {..}]) <- moduleToProb def toFix repair_target
          desc <- describeProblem def toFix
          fixes <- runGenMonad tESTGENCONF desc tESTSEED geneticSearchPlusPostprocessing
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) $ Set.toList fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes modul . getFixBinds) fixProgs
          fixDiffs @?= expected
    ]

genTests :: TestTree
genTests =
  testGroup
    "Generation tests"
    [ localOption (mkTimeout 120_000_000) $
        testCase "Repair TwoFixes" $ do
          let toFix = "tests/cases/TwoFixes.hs"
              repair_target = Nothing
              expected =
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

          setSeedGenSeed (tESTSEED + 5)
          (cc', mod, [tp@EProb {..}]) <- moduleToProb def toFix repair_target
          desc <- describeProblem def toFix
          fixes <- pseudoGeneticRepair def desc
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) $ Set.toList fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes mod . getFixBinds) fixProgs
          fixDiffs @?= expected,
      localOption (mkTimeout 75_000_000) $
        testCase "Repair ThreeFixes" $ do
          let toFix = "tests/cases/ThreeFixes.hs"
              repair_target = Nothing
              expected =
                map
                  unlines
                  [ [ "diff --git a/tests/cases/ThreeFixes.hs b/tests/cases/ThreeFixes.hs",
                      "--- a/tests/cases/ThreeFixes.hs",
                      "+++ b/tests/cases/ThreeFixes.hs",
                      "@@ -20,1 +20,1 @@ brokenPair = (1, 2, 3)",
                      "-brokenPair = (1, 2, 3)",
                      "+brokenPair = (3, 4, 5)"
                    ]
                  ]

          setSeedGenSeed (tESTSEED + 5)
          (cc', mod, [tp@EProb {..}]) <- moduleToProb def toFix repair_target
          desc <- describeProblem def toFix
          fixes <- pseudoGeneticRepair def desc
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) $ Set.toList fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes mod . getFixBinds) fixProgs
          fixDiffs @?= expected,
      localOption (mkTimeout 90_000_000) $
        testCase "Repair FourFixes" $ do
          let toFix = "tests/cases/FourFixes.hs"
              repair_target = Nothing
              expected =
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
          setSeedGenSeed (tESTSEED + 5)
          (cc', mod, [tp@EProb {..}]) <- moduleToProb def toFix repair_target
          desc <- describeProblem def toFix
          fixes <- pseudoGeneticRepair def desc
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) $ Set.toList fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes mod . getFixBinds) fixProgs
          fixDiffs @?= expected
    ]

main = defaultMain tests
