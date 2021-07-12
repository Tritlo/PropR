{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Maybe (isJust, mapMaybe)
import Data.Vector (fromList)
import Endemic (getExprFitCands)
import Endemic.Diff (applyFixes, getFixBinds, ppDiff)
import Endemic.Eval
import Endemic.Search (geneticSearchPlusPostprocessing, mkDefaultConf, runGenMonad)
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
    [properGenTests, genTests]

properGenTests :: TestTree
properGenTests =
  testGroup
    "proper generation tests"
    [ localOption (mkTimeout 180_000_000) $
        testCase "Repair ThreeFixes w/ randomness" $ do
          let dcc = defaultConf
              gc = pseudoGenConf dcc
              cc = dcc {pseudoGenConf = gc}
              toFix = "tests/ThreeFixes.hs"
              repair_target = Nothing
              expected =
                map
                  unlines
                  [ [ "diff --git a/tests/ThreeFixes.hs b/tests/ThreeFixes.hs",
                      "--- a/tests/ThreeFixes.hs",
                      "+++ b/tests/ThreeFixes.hs",
                      "@@ -20,1 +20,1 @@ brokenPair = (1, 2, 3)",
                      "-brokenPair = (1, 2, 3)",
                      "+brokenPair = (3, 4, 5)"
                    ]
                  ]

          (cc', modul, [tp@EProb {..}]) <- moduleToProb cc toFix repair_target
          expr_fit_cands <- collectStats $ getExprFitCands cc' $ noLoc $ HsLet NoExtField e_ctxt $ noLoc undefVar
          let gconf = mkDefaultConf 64 50 tp cc' expr_fit_cands
          fixes <- runGenMonad gconf 69420 geneticSearchPlusPostprocessing
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes modul . getFixBinds) fixProgs
          fixDiffs @?= expected
    ]

genTests =
  testGroup
    "Generation tests"
    [ localOption (mkTimeout 120_000_000) $
        testCase "Repair TwoFixes" $ do
          let dcc = defaultConf
              gc = pseudoGenConf dcc
              cc = dcc {pseudoGenConf = gc}
              toFix = "tests/TwoFixes.hs"
              repair_target = Nothing
              expected =
                map
                  unlines
                  [ [ "diff --git a/tests/TwoFixes.hs b/tests/TwoFixes.hs",
                      "--- a/tests/TwoFixes.hs",
                      "+++ b/tests/TwoFixes.hs",
                      "@@ -12,1 +12,1 @@ brokenPair = (1, 2)",
                      "-brokenPair = (1, 2)",
                      "+brokenPair = (3, 4)"
                    ]
                  ]

          (cc', mod, [tp@EProb {..}]) <- moduleToProb cc toFix repair_target
          fixes <- pseudoGeneticRepair cc' tp
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes mod . getFixBinds) fixProgs
          fixDiffs @?= expected,
      localOption (mkTimeout 75_000_000) $
        testCase "Repair ThreeFixes" $ do
          let dcc = defaultConf
              gc = pseudoGenConf dcc
              cc = dcc {pseudoGenConf = gc}
              toFix = "tests/ThreeFixes.hs"
              repair_target = Nothing
              expected =
                map
                  unlines
                  [ [ "diff --git a/tests/ThreeFixes.hs b/tests/ThreeFixes.hs",
                      "--- a/tests/ThreeFixes.hs",
                      "+++ b/tests/ThreeFixes.hs",
                      "@@ -20,1 +20,1 @@ brokenPair = (1, 2, 3)",
                      "-brokenPair = (1, 2, 3)",
                      "+brokenPair = (3, 4, 5)"
                    ]
                  ]

          (cc', mod, [tp@EProb {..}]) <- moduleToProb cc toFix repair_target
          fixes <- pseudoGeneticRepair cc' tp
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes mod . getFixBinds) fixProgs
          fixDiffs @?= expected,
      localOption (mkTimeout 90_000_000) $
        testCase "Repair FourFixes" $ do
          let dcc = defaultConf
              gc = pseudoGenConf dcc
              cc = dcc {pseudoGenConf = gc}
              toFix = "tests/FourFixes.hs"
              repair_target = Nothing
              expected =
                map
                  unlines
                  [ [ "diff --git a/tests/FourFixes.hs b/tests/FourFixes.hs",
                      "--- a/tests/FourFixes.hs",
                      "+++ b/tests/FourFixes.hs",
                      "@@ -24,1 +24,1 @@ brokenPair = (1, 2, 3, 4)",
                      "-brokenPair = (1, 2, 3, 4)",
                      "+brokenPair = (3, 4, 5, 6)"
                    ]
                  ]

          (cc', mod, [tp@EProb {..}]) <- moduleToProb cc toFix repair_target
          fixes <- pseudoGeneticRepair cc' tp
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes mod . getFixBinds) fixProgs
          fixDiffs @?= expected
    ]

main = defaultMain tests
