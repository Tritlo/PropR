{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module TestUtils where

import Data.Default
import Data.List (sort)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic
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
import GHC (tm_parsed_module)
import GhcPlugins (ppr, showSDocUnsafe)
import Test.Tasty
import Test.Tasty.HUnit

-- | Chosen fairly by Random.org
tESTSEED :: Int
tESTSEED = 703_039_772

tESTGENCONF :: GeneticConfiguration
tESTGENCONF = def {crossoverRate = 0.4, mutationRate = 0.1, dropRate = 0.25, iterations = 1_000}

mkSimpleModuleTest :: Integer -> TestName -> FilePath -> Maybe String -> TestTree
mkSimpleModuleTest timeout tag toFix repair_target =
  localOption (mkTimeout timeout) $
    testCase tag $ do
      expected <- readExpected toFix
      setSeedGenSeed tESTSEED
      (cc', mod, mb_prob) <- moduleToProb def toFix repair_target
      case mb_prob of
        Nothing -> [] @?= expected
        Just ExProb {..} -> error "not supported yet!"
        Just tp@EProb {..} -> do
          fixes <- repair cc' tp
          let fixProgs = map (eProgToEProgFix . applyFixToEProg e_prog) fixes
              diffs =
                map
                  ( concatMap ppDiff
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
                    unlines (map (showSDocUnsafe . ppr) fixes)
                  ]
          assertBool msg check

mkRepairTest ::
  Configuration ->
  (ProblemDescription -> IO (Set EFix)) ->
  Integer ->
  TestName ->
  FilePath ->
  TestTree
mkRepairTest conf how timeout tag file = mkRepairTest' conf how timeout tag file Nothing Nothing

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
  -- | A list of fixes. If nothing, fixes are read from the file
  Maybe [String] ->
  -- | Indices of the fixes to use, if only some of the fixes in the file should be used.
  Maybe [Int] ->
  TestTree
mkRepairTest' conf how timeout tag file mb_expected indices =
  localOption (mkTimeout timeout) $
    testCase tag $ do
      expected' <- case mb_expected of
        Just x -> return x
        Nothing -> readExpected file
      let expected = case indices of
            Just inds -> map (expected' !!) $ take (length expected') inds
            _ -> expected'

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