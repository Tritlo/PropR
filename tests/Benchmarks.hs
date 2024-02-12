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
import PropR (describeProblem, getExprFitCands)
import PropR.Configuration
import PropR.Diff (applyFixes, fixesToDiffs, getFixBinds, ppDiff)
import PropR.Eval
import PropR.Search
import PropR.Search.Exhaustive
import PropR.Search.PseudoGenetic (pseudoGeneticRepair)
import PropR.Traversals
import PropR.Types
import PropR.Util (collectStats, getStats, logOut, reportStats, reportStats', resetStats, showTime, time)
import Test.Tasty
import TestUtils

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ specialTests,
      defaultTests,
      nonInterpreted,
      nonPar,
      neitherParNorIntepreted
    ]

runGenRepair :: ProblemDescription -> IO (Set EFix)
runGenRepair desc = do
  runGenMonad tESTGENCONF desc tESTSEED geneticSearchPlusPostprocessing

mkGenConfTestEx :: Configuration -> Integer -> TestName -> FilePath -> TestTree
mkGenConfTestEx conf timeout tag file =
  mkRepairTest' conf runGenRepair timeout tag file def {reportTestStats = True}

defaultTests :: TestTree
defaultTests =
  testGroup
    "Interpreted and Paralell tests"
    [ mkGenConfTestEx conf 360_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkGenConfTestEx conf 360_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs"
    ]
  where
    conf = tESTCONF {compileConfig = cc {useInterpreted = True, parChecks = True}}
    cc = compileConfig tESTCONF

nonInterpreted :: TestTree
nonInterpreted =
  testGroup
    "Non-interpreted but Parallel tests"
    [ mkGenConfTestEx conf 360_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkGenConfTestEx conf 360_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs"
    ]
  where
    conf = tESTCONF {compileConfig = cc {useInterpreted = False, parChecks = True}}
    cc = compileConfig tESTCONF

nonPar :: TestTree
nonPar =
  testGroup
    "Non-par tests"
    [ mkGenConfTestEx conf 360_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkGenConfTestEx conf 360_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs"
    ]
  where
    conf = tESTCONF {compileConfig = cc {parChecks = False, useInterpreted = True}}
    cc = compileConfig tESTCONF

neitherParNorIntepreted :: TestTree
neitherParNorIntepreted =
  testGroup
    "Non-interpreted  non-interpreted tests"
    [ mkGenConfTestEx conf 360_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkGenConfTestEx conf 360_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs"
    ]
  where
    conf = tESTCONF {compileConfig = cc {parChecks = False, useInterpreted = False}}
    cc = compileConfig tESTCONF

specialTests :: TestTree
specialTests =
  testGroup
    "Special"
    [ mkRepairTest'
        tESTCONF {compileConfig = (compileConfig tESTCONF) {useInterpreted = False}}
        runGenRepair
        120_000_000
        "Non-interpreted with loop"
        "tests/cases/LoopBreaker.hs"
        def {reportTestStats = True}
    ]

main :: IO ()
main = defaultMain tests
