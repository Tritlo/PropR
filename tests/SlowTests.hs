{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Maybe (isJust, mapMaybe)
import Synth.Diff (applyFixes, getFixBinds, ppDiff)
import Synth.Eval
import Synth.Gen (genRepair)
import Synth.Replace (replaceExpr)
import Synth.Types
import Synth.Util
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Tests"
    [genTests]

genTests =
  testGroup
    "Generation tests"
    [ localOption (mkTimeout 60_000_000) $
        testCase "Repair TwoFixes" $ do
          let cc = defaultConf
              toFix = "tests/TwoFixes.hs"
              repair_target = Nothing
              expected =
                map
                  unlines
                  [ [ "---tests/TwoFixes.hs",
                      "+++tests/TwoFixes.hs",
                      "@@ -12,1 +12,1 @@ brokenPair = (1, 2)",
                      "-brokenPair = (1, 2)",
                      "+brokenPair = (3, 4)"
                    ]
                  ]

          (cc', mod, [tp@EProb {..}]) <- moduleToProb cc toFix repair_target
          fixes <- genRepair cc' tp
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes mod . getFixBinds) fixProgs
          fixDiffs @?= expected,
      localOption (mkTimeout 75_000_000) $
        testCase "Repair ThreeFixes" $ do
          let cc = defaultConf
              toFix = "tests/ThreeFixes.hs"
              repair_target = Nothing
              expected =
                map
                  unlines
                  [ [ "---tests/ThreeFixes.hs",
                      "+++tests/ThreeFixes.hs",
                      "@@ -20,1 +20,1 @@ brokenPair = (1, 2, 3)",
                      "-brokenPair = (1, 2, 3)",
                      "+brokenPair = (3, 4, 5)"
                    ]
                  ]

          (cc', mod, [tp@EProb {..}]) <- moduleToProb cc toFix repair_target
          fixes <- genRepair cc' tp
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes mod . getFixBinds) fixProgs
          fixDiffs @?= expected,
      localOption (mkTimeout 90_000_000) $
        testCase "Repair FourFixes" $ do
          let cc = defaultConf
              toFix = "tests/FourFixes.hs"
              repair_target = Nothing
              expected =
                map
                  unlines
                  [ [ "---tests/FourFixes.hs",
                      "+++tests/FourFixes.hs",
                      "@@ -24,1 +24,1 @@ brokenPair = (1, 2, 3, 4)",
                      "-brokenPair = (1, 2, 3, 4)",
                      "+brokenPair = (3, 4, 5, 6)"
                    ]
                  ]

          (cc', mod, [tp@EProb {..}]) <- moduleToProb cc toFix repair_target
          fixes <- genRepair cc' tp
          let fixProgs = map (`replaceExpr` progAtTy e_prog e_ty) fixes
              fixDiffs = map (concatMap ppDiff . snd . applyFixes mod . getFixBinds) fixProgs
          fixDiffs @?= expected
    ]

main = defaultMain tests
