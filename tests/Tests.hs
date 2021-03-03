{-# LANGUAGE NumericUnderscores, TypeApplications #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Synth.Repair (repair, failingProps, propCounterExample, runJustParseExpr)
import Synth.Eval ( CompileConfig(..), compileCheck, traceTarget
                  , showUnsafe, moduleToProb)
import Synth.Flatten
import Synth.Util

import Data.Bits (finiteBitSize)
import Data.Dynamic (fromDynamic)
import Data.Tree
import qualified Data.Map as Map
import Trace.Hpc.Mix
import Data.List (find)
import Data.Maybe (isJust)

import GhcPlugins (getLoc)


tests :: TestTree
tests = testGroup "Tests" [ utilTests
                          , repairTests
                          , failingPropsTests
                          , counterExampleTests
                          , traceTests
                          , moduleTests]

-- We can only do the inverse for ints up to 64, so we only support a maximum
-- of 64 props!
prop_BoolToBitsInverse :: [Bool] -> Property
prop_BoolToBitsInverse bs =
   length bs <= finiteBitSize (0 :: Int) ==>
    take (length bs) (bitToBools (boolsToBit bs)) == bs

utilTests :: TestTree
utilTests = testProperties "Utils" [
      ("dropPrefix", property prop_dropsPrefix)
    , ("startsWith", property prop_startsWith)
    , ("boolToBitsInverse", property prop_BoolToBitsInverse)
    ]

repairTests = testGroup "Repair" [
    -- A simple tests to see if we can repair (foldl (-) 0) to (foldl (+) 0)
    -- in a reasonable amount of time (here 10s)
      localOption (mkTimeout 10_000_000) $
        testCase "Repair `foldl (-) 0`" $ do
          let cc = CompConf {
                      hole_lvl=2,
                      packages = ["base", "process", "QuickCheck" ],
                      importStmts = ["import Prelude hiding (id, ($), ($!), asTypeOf)"]}
              ty = "[Int] -> Int"
              wrong_prog = "(foldl (-) 0)"
              props = ["prop_isSum f xs = f xs == sum xs"]
              context = [ "zero = 0 :: Int"
                        , "one = 1 :: Int"
                        , "add = (+) :: Int -> Int -> Int"]
              expected = "((foldl (+) 0)) :: [Int] -> Int"
          fixes <- map trim <$> repair cc props context ty wrong_prog
          expected `elem` fixes @? "Expected repair not found in fixes"
    , localOption (mkTimeout 15_000_000) $
        testCase "Repair `gcd'` with gcd" $ do
          let cc = CompConf {
                      hole_lvl=0,
                      packages = ["base", "process", "QuickCheck" ],
                      importStmts = ["import Prelude"]}
              props = [ "prop_1 f = f 0 55 == 55"
                      , "prop_2 f = f 1071 1029 == 21"]
              ty = "Int -> Int -> Int"
              wrong_prog = unlines [
                          "let { gcd' 0 b = gcd' 0 b",
                          "    ; gcd' a b | b == 0 = a",
                          "    ; gcd' a b = if (a > b) then gcd' (a-b) b else gcd' a (b-a)}",
                          "     in gcd'"]
              context = []
          fixes <- map trim <$> repair cc props context ty wrong_prog
          (length fixes > 0) @? "No fix found"
  ]

failingPropsTests = testGroup "Failing props" [
    localOption (mkTimeout 15_000_000) $
        testCase "Failing props for gcd" $ do
          let cc = CompConf {
                      hole_lvl=0,
                      packages = ["base", "process", "QuickCheck" ],
                      importStmts = ["import Prelude"]}
              props :: [String]
              props = [ "prop_1 f = f 0 55 == 55"
                      , "prop_2 f = f 1071 1029 == 21"]
              ty = "Int -> Int -> Int"
              wrong_prog = unlines [
                          "let { gcd' 0 b = gcd' 0 b",
                          "    ; gcd' a b | b == 0 = a",
                          "    ; gcd' a b = if (a > b) then gcd' (a-b) b else gcd' a (b-a)}",
                          "     in gcd'"]
              context = []
          failed_props <- failingProps cc props context ty wrong_prog
          -- Only the first prop should be failing (due to an infinite loop)
          failed_props @?= [head props]
      , localOption (mkTimeout 10_000_000) $
          testCase "Only one failing prop" $ do
            let cc = CompConf {
                        hole_lvl=2,
                        packages = ["base", "process", "QuickCheck" ],
                        importStmts = ["import Prelude hiding (id, ($), ($!), asTypeOf)"]}
                ty = "[Int] -> Int"
                wrong_prog = "(foldl (-) 0)"
                props = ["prop_isSum f xs = f xs == sum xs"]
                context = [ "zero = 0 :: Int"
                          , "one = 1 :: Int"
                          , "add = (+) :: Int -> Int -> Int"]
                expected = "((foldl (+) 0)) :: [Int] -> Int"
            failed_props <- failingProps cc props context ty wrong_prog
            failed_props @?= props
    ]

counterExampleTests = testGroup "Counter Examples" [
      localOption (mkTimeout 10_000_000) $
          testCase "Only one counter example" $ do
            let cc = CompConf {
                        hole_lvl=2,
                        packages = ["base", "process", "QuickCheck" ],
                        importStmts = ["import Prelude hiding (id, ($), ($!), asTypeOf)"]}
                ty = "[Int] -> Int"
                wrong_prog = "(foldl (-) 0)"
                props = ["prop_isSum f xs = f xs == sum xs"]
                context = []
                expected = "((foldl (+) 0)) :: [Int] -> Int"
            [failed_prop] <- failingProps cc props context ty wrong_prog
            Just [counter_example] <- propCounterExample cc context ty wrong_prog failed_prop
            res <- compileCheck cc ("(foldl (-) 0) " ++ counter_example ++ " == sum " ++ counter_example)
            case fromDynamic @Bool res of
              Just v -> not v @? "Counter Example is not a counter example!"
              Nothing -> error "Incorrect type!!"

    , localOption (mkTimeout 10_000_000) $
          testCase "Multiple examples" $ do
            let cc = CompConf {
                        hole_lvl=2,
                        packages = ["base", "process", "QuickCheck" ],
                        importStmts = ["import Prelude hiding (id, ($), ($!), asTypeOf)"]}
                ty = "Int -> Int -> Int"
                wrong_prog = "(-)"
                props = ["prop_isPlus f a b = f a b == (a + b)"]
                context = []
            [failed_prop] <- failingProps cc props context ty wrong_prog
            Just counter_example_args <- propCounterExample cc context ty wrong_prog failed_prop
            let arg_str = unwords counter_example_args
            res <- compileCheck cc ("(-) " ++ arg_str ++ " == (+) " ++ arg_str)
            case fromDynamic @Bool res of
              Just v -> not v @? "Counter Example is not a counter example!"
              Nothing -> error "Incorrect type!!"
  ,  localOption (mkTimeout 15_000_000) $
        testCase "No args loop fail" $ do
          let cc = CompConf {
                      hole_lvl=0,
                      packages = ["base", "process", "QuickCheck" ],
                      importStmts = ["import Prelude"]}
              props :: [String]
              props = [ "prop_1 f = f 0 55 == 55"
                      , "prop_2 f = f 1071 1029 == 21"]
              ty = "Int -> Int -> Int"
              wrong_prog = unlines [
                          "let { gcd' 0 b = gcd' 0 b",
                          "    ; gcd' a b | b == 0 = a",
                          "    ; gcd' a b = if (a > b) then gcd' (a-b) b else gcd' a (b-a)}",
                          "     in gcd'"]
              context = []
          [failed_prop] <- failingProps cc props context ty wrong_prog
          -- Only the first prop should be failing (due to an infinite loop)
          failed_prop @?= head props
          Just counter_example_args <- propCounterExample cc context ty wrong_prog failed_prop
          null counter_example_args @? "The counter example should not have any arguments!"
  ]


traceTests = testGroup "Trace tests" [
      localOption (mkTimeout 10_000_000) $
          testCase "Trace foldl" $ do
            let cc = CompConf {
                        hole_lvl=0,
                        packages = ["base", "process", "QuickCheck" ],
                        importStmts = ["import Prelude hiding (id, ($), ($!), asTypeOf)"]}
                ty = "[Int] -> Int"
                wrong_prog = "(foldl (-) 0)"
                props = ["prop_isSum f xs = f xs == sum xs"]
                context = []
                expected = "((foldl (+) 0)) :: [Int] -> Int"
            [failed_prop] <- failingProps cc props context ty wrong_prog
            Just counter_example <- propCounterExample cc context ty wrong_prog failed_prop
            Just (Node{subForest=[tree@Node{rootLabel=(tl, tname)}]})
                 <- traceTarget cc wrong_prog failed_prop counter_example
            expr <- runJustParseExpr cc wrong_prog
            (getLoc expr) @?= mkInteractive tl
            (all (== 1) $ map snd $ concatMap snd $ flatten tree) @? "All subexpressions should be touched only once!"
  ,  localOption (mkTimeout 30_000_000) $
        testCase "Trace finds loop" $ do
          let cc = CompConf {
                      hole_lvl=0,
                      packages = ["base", "process", "QuickCheck" ],
                      importStmts = ["import Prelude"]}
              props :: [String]
              props = [ "prop_1 f = f 0 55 == 55"
                      , "prop_2 f = f 1071 1029 == 21"]
              ty = "Int -> Int -> Int"
              wrong_prog = unlines [
                          "let { gcd' 0 b = gcd' 0 b",
                          "    ; gcd' a b | b == 0 = a",
                          "    ; gcd' a b = if (a > b) then gcd' (a-b) b else gcd' a (b-a)}",
                          "     in gcd'"]
              context = []
          [failed_prop] <- failingProps cc props context ty wrong_prog
          Just counter_example_args <- propCounterExample cc context ty wrong_prog failed_prop
          -- We generate the trace
          Just res <- traceTarget cc wrong_prog failed_prop counter_example_args
          parsed <- runJustParseExpr cc wrong_prog
          let eMap = Map.fromList $ map (\e -> (getLoc e, showUnsafe e)) $ flattenExpr parsed
              trc = map (\(s,r) -> (s, eMap Map.!? (mkInteractive s), r, maximum $ map snd r)) $ flatten res
              isXbox (ExpBox _) = True
              isXBox _ = False
              isInEMapOrNotExpBox (_, Just _, _, _) = True
              isInEMapOrNotExpBox (_, _, r, _) = not (any isXBox $ map fst r)
              isLooper (_, Just "gcd' 0 b",_, _) = True
              isLooper _ = False

              loopEntry = find isLooper trc

          all isInEMapOrNotExpBox trc @? "All the expressions should be present in the trace!"
          isJust loopEntry @? "The loop causing expresssion should be in the trace"
          let Just (_,_,_,loops) = loopEntry
          loops >= 100_000 @? "There should have been a lot of invocations of the loop!"





  ]


moduleTests = testGroup "Module tests" [
  localOption (mkTimeout 30_000_000) $
    testCase "Repair BrokenModule" $ do
      let cc = CompConf {
                 hole_lvl=0,
                 packages = ["base", "process", "QuickCheck" ],
                 importStmts = ["import Prelude"]}
          toFix = "tests/BrokenModule.hs"
          repair_target = Just "broken"
      (cc', context, wrong_prog, ty, props) <- moduleToProb cc toFix repair_target
      fixes <- repair cc' props context ty wrong_prog
      not (null fixes) @? "Repairs for foldl should work!"
  ]

main = defaultMain tests
