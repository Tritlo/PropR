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
runGenRepair desc = runGenMonad tESTGENCONF desc tESTSEED geneticSearchPlusPostprocessing

mkGenConfTestEx :: Configuration -> Integer -> TestName -> FilePath -> TestTree
mkGenConfTestEx conf = mkRepairTest conf runGenRepair

defaultTests :: TestTree
defaultTests =
  testGroup
    "Interpreted and Paralell tests"
    [ mkGenConfTestEx def 180_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkGenConfTestEx def 180_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs"
    ]
  where
    conf = def {compileConfig = def {useInterpreted = True, parChecks = True}}

nonInterpreted :: TestTree
nonInterpreted =
  testGroup
    "Non-interpreted but Parallel tests"
    [ mkGenConfTestEx conf 180_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkGenConfTestEx conf 180_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs"
    ]
  where
    conf = def {compileConfig = def {useInterpreted = False, parChecks = True}}

nonPar :: TestTree
nonPar =
  testGroup
    "Non-par tests"
    [ mkGenConfTestEx conf 180_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkGenConfTestEx conf 180_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs"
    ]
  where
    conf = def {compileConfig = def {parChecks = False, useInterpreted = True}}

neitherParNorIntepreted :: TestTree
neitherParNorIntepreted =
  testGroup
    "Non-interpreted  non-interpreted tests"
    [ mkGenConfTestEx conf 180_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkGenConfTestEx conf 180_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs"
    ]
  where
    conf = def {compileConfig = def {parChecks = False, useInterpreted = False}}

specialTests :: TestTree
specialTests =
  testGroup
    "Special"
    [ mkRepairTest
        def {compileConfig = def {useInterpreted = False}}
        runGenRepair
        120_000_000
        "Non-interpreted with loop"
        "tests/cases/LoopBreaker.hs"
    ]

main :: IO ()
main = defaultMain tests
