{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Maybe (isJust, mapMaybe)
import Endemic (getExprFitCands)
import Endemic.Diff (applyFixes, getFixBinds, ppDiff)
import Endemic.Eval
import Endemic.Search ( geneticSearchPlusPostprocessing, runGenMonad
                      , ProblemDescription(..), describeProblem )
import Endemic.Search.PseudoGenetic (pseudoGeneticRepair)
import Endemic.Configuration
import Endemic.Traversals
import Endemic.Types
import Endemic.Util
import GHC (HsExpr (HsLet), NoExtField (NoExtField))
import GhcPlugins (noLoc)
import Test.Tasty
import Test.Tasty.HUnit
import Data.Default

tests :: TestTree
tests =
  testGroup
    "Tests"
    [properGenTests, genTests]

properGenTests :: TestTree
properGenTests =
  testGroup
    "proper generation tests"
    [ localOption (mkTimeout 180_000_000) $
        testCase "Repair ThreeFixes w/ randomness" $ do
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

          (_, modul, [EProb{..}]) <- moduleToProb def toFix repair_target
          desc <- describeProblem def toFix
          fixes <- runGenMonad def desc 69420 geneticSearchPlusPostprocessing
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes modul . getFixBinds) fixProgs
          fixDiffs @?= expected
    ]

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

          (cc', mod, [tp@EProb {..}]) <- moduleToProb def toFix repair_target
          fixes <- pseudoGeneticRepair def {searchAlgorithm = PseudoGenetic def} tp
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
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

          (cc', mod, [tp@EProb {..}]) <- moduleToProb def toFix repair_target
          fixes <- pseudoGeneticRepair def {searchAlgorithm = PseudoGenetic def} tp
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
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

          (cc', mod, [tp@EProb {..}]) <- moduleToProb def toFix repair_target
          fixes <- pseudoGeneticRepair def {searchAlgorithm = PseudoGenetic def} tp
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes mod . getFixBinds) fixProgs
          fixDiffs @?= expected
    ]

main = defaultMain tests
