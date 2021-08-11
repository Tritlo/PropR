{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Data.Default
import Data.Functor (($>))
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
import Endemic.Util (logStr, withLogLevel)
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import TestUtils

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ specialTests,
      tastyFixTests,
      randTests,
      properGenTests,
      genTests,
      exhaustiveTests,
      refinementTests
    ]

runGenRepair :: ProblemDescription -> IO (Set EFix)
runGenRepair desc = runGenMonad tESTGENCONF desc tESTSEED geneticSearchPlusPostprocessing

runGenRepair' :: GeneticConfiguration -> ProblemDescription -> IO (Set EFix)
runGenRepair' gen_conf desc = runGenMonad gen_conf desc tESTSEED geneticSearchPlusPostprocessing

mkGenConfTestEx :: Integer -> TestName -> FilePath -> TestTree
mkGenConfTestEx = mkRepairTest def runGenRepair

mkSearchTestExPartial :: SearchAlgorithm -> Integer -> TestName -> FilePath -> Maybe [Int] -> TestTree
mkSearchTestExPartial search_conf timeout tag file indices =
  mkRepairTest' def (runRepair search_conf) timeout tag file def {indices = indices}

mkSearchTestEx :: SearchAlgorithm -> Integer -> TestName -> FilePath -> TestTree
mkSearchTestEx search_conf = mkRepairTest def (runRepair search_conf)

mkPseudoGenTestEx :: Integer -> TestName -> FilePath -> TestTree
mkPseudoGenTestEx = mkRepairTest def (pseudoGeneticRepair def)

tastyFixTests :: TestTree
tastyFixTests =
  testGroup
    "Tasty fix tests"
    [ mkGenConfTestEx 180_000_000 "Repair TastyFix" "tests/cases/TastyFix.hs",
      mkGenConfTestEx 180_000_000 "Repair TastyMix" "tests/cases/TastyMix.hs",
      mkGenConfTestEx 180_000_000 "Repair TastyTwoFix" "tests/cases/TastyTwoFix.hs"
    ]

randTests :: TestTree
randTests =
  testGroup
    "Random search tests"
    [ let conf = Random def {randStopOnResults = True, randIgnoreFailing = True}
       in mkSearchTestExPartial conf 180_000_000 "Repair TastyFix" "tests/cases/TastyFix.hs" (Just [1])
    ]

exhaustiveTests :: TestTree
exhaustiveTests =
  testGroup
    "Exhaustive search tests"
    [ let conf = Exhaustive def {exhStopOnResults = True}
       in mkSearchTestEx conf 180_000_000 "Repair TastyFix" "tests/cases/TastyFix.hs",
      let conf = Exhaustive def {exhStopOnResults = True}
       in mkSearchTestEx conf 180_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs"
    ]

properGenTests :: TestTree
properGenTests =
  testGroup
    "Genetic search tests"
    [ mkGenConfTestEx 180_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkGenConfTestEx 180_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs",
      -- With all the new fixes, we need to bump the population size
      mkGenConfTestEx 180_000_000 "Repair FourFixes" "tests/cases/FourFixes.hs"
      --mkRepairTest def (runGenRepair' tESTGENCONF {populationSize = 92}) 240_000_000 "Repair FourFixes" "tests/cases/FourFixes.hs"
    ]

genTests :: TestTree
genTests =
  testGroup
    "PseudoGenetic search tests"
    [ mkPseudoGenTestEx 240_000_000 "Repair TwoFixes" "tests/cases/TwoFixes.hs",
      mkPseudoGenTestEx 180_000_000 "Repair ThreeFixes" "tests/cases/ThreeFixes.hs",
      mkPseudoGenTestEx 180_000_000 "Repair FourFixes" "tests/cases/FourFixes.hs"
    ]

-- These all test compilation corner cases
specialTests :: TestTree
specialTests =
  testGroup
    "Special"
    [ mkGenConfTestEx 30_000_000 "Repair UsesDependency" "tests/cases/UsesDependency.hs",
      mkGenConfTestEx 30_000_000 "Repair Operators" "tests/cases/Operators.hs",
      mkGenConfTestEx 30_000_000 "Repair Data" "tests/cases/Data.hs",
      mkGenConfTestEx 180_000_000 "Repair Multi" "tests/cases/Multi.hs",
      mkGenConfTestEx 30_000_000 "Repair DoubleDecl" "tests/cases/DoubleDecl.hs",
      mkGenConfTestEx 60_000_000 "All props pass" "tests/cases/AllPropsPass.hs",
      mkGenConfTestEx 60_000_000 "No props" "tests/cases/NoProps.hs",
      mkRepairTest
        def {compileConfig = def {useInterpreted = False}}
        runGenRepair
        60_000_000
        "Non-Interpreted"
        "tests/cases/LoopBreaker.hs",
      mkRepairTest
        def {compileConfig = def {useInterpreted = True, parChecks = False}}
        runGenRepair
        120_000_000
        "Interpreted Non-Par"
        "tests/cases/LoopBreaker.hs",
      mkRepairTest
        def {compileConfig = def {useInterpreted = True, parChecks = True}}
        runGenRepair
        240_000_000
        "Interpreted Par"
        "tests/cases/LoopBreaker.hs",
      mkGenConfTestEx 60_000_000 "Wrapped fits" "tests/cases/Wrap.hs",
      mkGenConfTestEx 60_000_000 "Ambiguous fits" "tests/cases/AmbiguousTypeVariables.hs",
      mkRepairTest'
        def {compileConfig = def {excludeTargets = ["brokenPair"]}}
        runGenRepair
        5_000_000
        "Exclude targets"
        "tests/cases/ThreeFixes.hs"
        def {mb_expected = Just []},
      mkGenConfTestEx 60_000_000 "ExprWhere fits" "tests/cases/ExprWhere.hs",
      mkGenConfTestEx 60_000_000 "DefaultingFixes" "tests/cases/DefaultingFixes.hs",
      mkGenConfTestEx 60_000_000 "Issue 88" "tests/cases/Issue88.hs"
    ]

refinementTests :: TestTree
refinementTests =
  testGroup
    "Refinments"
    [ mkRepairTest'
        ( def
            { compileConfig = def {holeLvl = 2},
              logConfig = def {logLoc = True}
            }
        )
        runGenRepair
        240_000_000
        "Refinement test"
        "tests/cases/SimpleRefinement.hs"
        def {allowMix = True}
    ]

main :: IO ()
main = defaultMain tests
