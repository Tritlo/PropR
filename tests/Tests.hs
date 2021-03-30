{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow
import Data.Bits (finiteBitSize)
import Data.Dynamic (fromDynamic)
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Tree
import Data.Tuple (swap)
import GhcPlugins (GenLocated (L), getLoc, unLoc)
import Synth.Diff
import Synth.Eval
  ( CompileConfig (..),
    buildTraceCorrel,
    compileParsedCheck,
    moduleToProb,
    runJustParseExpr,
    showUnsafe,
    traceTarget,
  )
import Synth.Flatten
import Synth.Repair
  ( failingProps,
    getExprFitCands,
    propCounterExample,
    repair,
    translate,
  )
import Synth.Sanctify
import Synth.Types
import Synth.Util
import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Trace.Hpc.Mix

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ utilTests,
      repairTests,
      failingPropsTests,
      counterExampleTests,
      traceTests,
      moduleTests,
      sanctifyTests
    ]

-- We can only do the inverse for ints up to 64, so we only support a maximum
-- of 64 props!
prop_BoolToBitsInverse :: [Bool] -> Property
prop_BoolToBitsInverse bs =
  length bs <= finiteBitSize (0 :: Int)
    ==> take (length bs) (bitToBools (boolsToBit bs)) == bs

prop_insertAt :: Eq a => Int -> a -> [a] -> Property
prop_insertAt n a as = abs n < length as ==> insertAt n' a as !! n' == a
  where
    n' = n `mod` length as

prop_applToEach :: Int -> [a] -> Property
prop_applToEach n xs =
  n >= 0 ==> length app == n * lxs
    && length (concatMap snd app) == n * lxs * lxs
  where
    tl = zip [0 ..] . replicate n
    lxs = length xs
    app = applToEach tl xs

utilTests :: TestTree
utilTests =
  testProperties
    "Utils"
    [ ("dropPrefix", property prop_dropsPrefix),
      ("startsWith", property prop_startsWith),
      ("boolToBitsInverse", property prop_BoolToBitsInverse),
      ("oneAndRest", property (prop_oneAndRest @Integer)),
      ("insertAt", property (prop_insertAt @Integer)),
      ("applToEach", property (prop_applToEach @Integer))
    ]

repairTests =
  testGroup
    "Repair"
    [ -- A simple tests to see if we can repair (foldl (-) 0) to (foldl (+) 0)
      -- in a reasonable amount of time (here 10s)
      localOption (mkTimeout 10_000_000) $
        testCase "Repair `foldl (-) 0`" $ do
          let cc =
                CompConf
                  { hole_lvl = 2,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude hiding (id, ($), ($!), asTypeOf)"]
                  }
              ty = "[Int] -> Int"
              wrong_prog = "(foldl (-) 0)"
              props = ["prop_isSum f xs = f xs == sum xs"]
              context =
                [ "zero = 0 :: Int",
                  "one = 1 :: Int",
                  "add = (+) :: Int -> Int -> Int"
                ]
              expected = "((foldl (+) 0)) :: [Int] -> Int"
              rp =
                RProb
                  { r_target = "",
                    r_ctxt = context,
                    r_ty = ty,
                    r_prog = wrong_prog,
                    r_props = props
                  }
          fixes <- map trim <$> (translate cc rp >>= repair cc)
          expected `elem` fixes @? "Expected repair not found in fixes",
      localOption (mkTimeout 20_000_000) $
        testCase "GetExprCands finds important candidates" $ do
          let cc =
                CompConf
                  { hole_lvl = 0,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude"]
                  }
              wrong_prog =
                unlines
                  [ "let { gcd' 0 b = gcd' 0 b",
                    "    ; gcd' a b | b == 0 = a",
                    "    ; gcd' a b = if (a > b) then gcd' (a-b) b else gcd' a (b-a)",
                    "    ; a_constant_string = \"hello, world!\"}",
                    "     in gcd'"
                  ]
              expected =
                [ "EFC {\"hello, world!\"}",
                  "EFC {gcd' 0 b}",
                  "EFC {gcd' 0}",
                  "EFC {0}",
                  "EFC {b == 0}",
                  "EFC {0}",
                  "EFC {if (a > b) then gcd' (a - b) b else gcd' a (b - a)}",
                  "EFC {a > b}",
                  "EFC {gcd' (a - b) b}",
                  "EFC {gcd' (a - b)}",
                  "EFC {a - b}",
                  "EFC {gcd' a (b - a)}",
                  "EFC {gcd' a}",
                  "EFC {b - a}"
                ]
          expr_cands <- runJustParseExpr cc wrong_prog >>= getExprFitCands cc
          map showUnsafe expr_cands @?= expected,
      localOption (mkTimeout 20_000_000) $
        testCase "Repair `gcd'` with gcd" $ do
          let cc =
                CompConf
                  { hole_lvl = 0,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude"]
                  }
              props =
                [ "prop_1 f = f 0 55 == 55",
                  "prop_2 f = f 1071 1029 == 21"
                ]
              ty = "Int -> Int -> Int"
              wrong_prog =
                unlines
                  [ "let { gcd' 0 b = gcd' 0 b",
                    "    ; gcd' a b | b == 0 = a",
                    "    ; gcd' a b = if (a > b) then gcd' (a-b) b else gcd' a (b-a)}",
                    "     in gcd'"
                  ]
              context = []
              rp =
                RProb
                  { r_target = "",
                    r_ctxt = context,
                    r_ty = ty,
                    r_prog = wrong_prog,
                    r_props = props
                  }
          fixes <- map trim <$> (translate cc rp >>= repair cc)
          not (null fixes) @? "No fix found"
    ]

failingPropsTests =
  testGroup
    "Failing props"
    [ localOption (mkTimeout 15_000_000) $
        testCase "Failing props for gcd" $ do
          let cc =
                CompConf
                  { hole_lvl = 0,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude"]
                  }
              props :: [String]
              props =
                [ "prop_1 f = f 0 55 == 55",
                  "prop_2 f = f 1071 1029 == 21"
                ]
              ty = "Int -> Int -> Int"
              wrong_prog =
                unlines
                  [ "let { gcd' 0 b = gcd' 0 b",
                    "    ; gcd' a b | b == 0 = a",
                    "    ; gcd' a b = if (a > b) then gcd' (a-b) b else gcd' a (b-a)}",
                    "     in gcd'"
                  ]
              context = []
              rp =
                RProb
                  { r_target = "",
                    r_ctxt = context,
                    r_ty = ty,
                    r_prog = wrong_prog,
                    r_props = props
                  }
          tp <- translate cc rp
          failed_props <- failingProps cc tp
          -- Only the first prop should be failing (due to an infinite loop)
          map showUnsafe failed_props @?= [head props],
      localOption (mkTimeout 10_000_000) $
        testCase "Only one failing prop" $ do
          let cc =
                CompConf
                  { hole_lvl = 2,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude hiding (id, ($), ($!), asTypeOf)"]
                  }
              ty = "[Int] -> Int"
              wrong_prog = "(foldl (-) 0)"
              props = ["prop_isSum f xs = f xs == sum xs"]
              context =
                [ "zero = 0 :: Int",
                  "one = 1 :: Int",
                  "add = (+) :: Int -> Int -> Int"
                ]
              expected = "((foldl (+) 0)) :: [Int] -> Int"
              rp =
                RProb
                  { r_target = "",
                    r_ctxt = context,
                    r_ty = ty,
                    r_prog = wrong_prog,
                    r_props = props
                  }
          tp <- translate cc rp
          failed_props <- failingProps cc tp
          map showUnsafe failed_props @?= props
    ]

counterExampleTests =
  testGroup
    "Counter Examples"
    [ localOption (mkTimeout 10_000_000) $
        testCase "Only one counter example" $ do
          let cc =
                CompConf
                  { hole_lvl = 2,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude hiding (id, ($), ($!), asTypeOf)"]
                  }
              ty = "[Int] -> Int"
              wrong_prog = "(foldl (-) 0)"
              props = ["prop_isSum f xs = f xs == sum xs"]
              context = []
              expected = "((foldl (+) 0)) :: [Int] -> Int"
              rp =
                RProb
                  { r_target = "",
                    r_ctxt = context,
                    r_ty = ty,
                    r_prog = wrong_prog,
                    r_props = props
                  }
          tp <- translate cc rp
          [failed_prop] <- failingProps cc tp
          Just [counter_example] <- propCounterExample cc tp failed_prop
          let expr = "(foldl (-) 0) " ++ counter_example ++ " == sum " ++ counter_example
          res <- runJustParseExpr cc expr >>= compileParsedCheck cc
          case fromDynamic @Bool res of
            Just v -> not v @? "Counter Example is not a counter example!"
            Nothing -> error "Incorrect type!!",
      localOption (mkTimeout 10_000_000) $
        testCase "Multiple examples" $ do
          let cc =
                CompConf
                  { hole_lvl = 2,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude hiding (id, ($), ($!), asTypeOf)"]
                  }
              ty = "Int -> Int -> Int"
              wrong_prog = "(-)"
              props = ["prop_isPlus f a b = f a b == (a + b)"]
              context = []
              rp =
                RProb
                  { r_target = "",
                    r_ctxt = context,
                    r_ty = ty,
                    r_prog = wrong_prog,
                    r_props = props
                  }
          tp <- translate cc rp
          [failed_prop] <- failingProps cc tp
          Just counter_example_args <- propCounterExample cc tp failed_prop
          let arg_str = unwords counter_example_args
              expr = "(-) " ++ arg_str ++ " == (+) " ++ arg_str
          res <- runJustParseExpr cc expr >>= compileParsedCheck cc
          case fromDynamic @Bool res of
            Just v -> not v @? "Counter Example is not a counter example!"
            Nothing -> error "Incorrect type!!",
      localOption (mkTimeout 15_000_000) $
        testCase "No args loop fail" $ do
          let cc =
                CompConf
                  { hole_lvl = 0,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude"]
                  }
              props :: [String]
              props =
                [ "prop_1 f = f 0 55 == 55",
                  "prop_2 f = f 1071 1029 == 21"
                ]
              ty = "Int -> Int -> Int"
              wrong_prog =
                unlines
                  [ "let { gcd' 0 b = gcd' 0 b",
                    "    ; gcd' a b | b == 0 = a",
                    "    ; gcd' a b = if (a > b) then gcd' (a-b) b else gcd' a (b-a)}",
                    "     in gcd'"
                  ]
              context = []
              rp =
                RProb
                  { r_target = "",
                    r_ctxt = context,
                    r_ty = ty,
                    r_prog = wrong_prog,
                    r_props = props
                  }
          tp <- translate cc rp
          [failed_prop] <- failingProps cc tp
          -- Only the first prop should be failing (due to an infinite loop)
          showUnsafe failed_prop @?= head props
          Just counter_example_args <- propCounterExample cc tp failed_prop
          null counter_example_args @? "The counter example should not have any arguments!"
    ]

traceTests =
  testGroup
    "Trace tests"
    [ localOption (mkTimeout 10_000_000) $
        testCase "Trace foldl" $ do
          let cc =
                CompConf
                  { hole_lvl = 0,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude hiding (id, ($), ($!), asTypeOf)"]
                  }
              ty = "[Int] -> Int"
              wrong_prog = "(foldl (-) 0)"
              props = ["prop_isSum f xs = f xs == sum xs"]
              context = []
              expected = "((foldl (+) 0)) :: [Int] -> Int"
              rp =
                RProb
                  { r_target = "",
                    r_ctxt = context,
                    r_ty = ty,
                    r_prog = wrong_prog,
                    r_props = props
                  }
          tp@EProb {..} <- translate cc rp
          [failed_prop] <- failingProps cc tp
          Just counter_example <- propCounterExample cc tp failed_prop
          Just Node {subForest = [tree@Node {rootLabel = (tl, tname)}]} <-
            traceTarget cc e_prog failed_prop counter_example
          expr <- runJustParseExpr cc wrong_prog
          getLoc expr @?= mkInteractive tl
          all ((== 1) . snd) (concatMap snd $ flatten tree) @? "All subexpressions should be touched only once!",
      localOption (mkTimeout 30_000_000) $
        testCase "Trace finds loop" $ do
          let cc =
                CompConf
                  { hole_lvl = 0,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude"]
                  }
              props :: [String]
              props =
                [ "prop_1 f = f 0 55 == 55",
                  "prop_2 f = f 1071 1029 == 21"
                ]
              ty = "Int -> Int -> Int"
              wrong_prog =
                unlines
                  [ "let { gcd' 0 b = gcd' 0 b",
                    "    ; gcd' a b | b == 0 = a",
                    "    ; gcd' a b = if (a > b) then gcd' (a-b) b else gcd' a (b-a)}",
                    "     in gcd'"
                  ]
              context = []
              rp =
                RProb
                  { r_target = "",
                    r_ctxt = context,
                    r_ty = ty,
                    r_prog = wrong_prog,
                    r_props = props
                  }
          tp@EProb {..} <- translate cc rp
          [failed_prop] <- failingProps cc tp
          Just counter_example_args <- propCounterExample cc tp failed_prop
          -- We generate the trace
          let prog_at_ty = progAtTy e_prog e_ty
          (_, tcorrel) <- buildTraceCorrel cc prog_at_ty
          Just res <- traceTarget cc prog_at_ty failed_prop counter_example_args
          let eMap = Map.fromList $ map (getLoc &&& showUnsafe) $ flattenExpr prog_at_ty
              chain l = tcorrel Map.!? l >>= (eMap Map.!?)
              trc =
                map (\(s, r) -> (chain $ mkInteractive s, r, maximum $ map snd r)) $
                  flatten res
              isXbox (ExpBox _) = True
              isXBox _ = False
              isInEMapOrNotExpBox (Just _, _, _) = True
              isInEMapOrNotExpBox (_, r, _) = not (any (isXBox . fst) r)
              isLooper (Just "gcd' 0 b", _, _) = True
              isLooper _ = False

              loopEntry = find isLooper trc

          all isInEMapOrNotExpBox trc @? "All the expressions should be present in the trace!"
          isJust loopEntry @? "The loop causing expresssion should be in the trace"
          let Just (_, _, loops) = loopEntry
          loops >= 100_000 @? "There should have been a lot of invocations of the loop!"
    ]

sanctifyTests =
  testGroup
    "Sanctify tests"
    [ localOption (mkTimeout 1_000_000) $
        testCase "Sanctify foldl program" $ do
          let cc =
                CompConf
                  { hole_lvl = 0,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude"]
                  }
              toFix = "tests/BrokenModule.hs"
              repair_target = Just "broken"
          (cc', _, [RProb {..}]) <- moduleToProb cc toFix repair_target
          expr <- runJustParseExpr cc' r_prog
          -- There are 7 ways to replace parts of the broken function in BrokenModule
          -- with holes:
          length (sanctifyExpr expr) @?= 7
    ]

moduleTests =
  testGroup
    "Module tests"
    [ localOption (mkTimeout 30_000_000) $
        testCase "Repair BrokenModule With Diff" $ do
          let cc =
                CompConf
                  { hole_lvl = 0,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude"]
                  }
              toFix = "tests/BrokenModule.hs"
              repair_target = Just "broken"
              expected =
                map
                  unlines
                  [ [ "---tests/BrokenModule.hs",
                      "+++tests/BrokenModule.hs",
                      "@@ -8,1 +8,1 @@ broken = foldl (-) 0",
                      "-broken = foldl (-) 0",
                      "+broken = sum"
                    ],
                    [ "---tests/BrokenModule.hs",
                      "+++tests/BrokenModule.hs",
                      "@@ -8,1 +8,1 @@ broken = foldl (-) 0",
                      "-broken = foldl (-) 0",
                      "+broken = foldl add 0"
                    ],
                    [ "---tests/BrokenModule.hs",
                      "+++tests/BrokenModule.hs",
                      "@@ -8,1 +8,1 @@ broken = foldl (-) 0",
                      "-broken = foldl (-) 0",
                      "+broken = foldl (+) 0"
                    ]
                  ]

          (cc', mod, [rp]) <- moduleToProb cc toFix repair_target
          fixes <- (translate cc' rp >>= repair cc') >>= mapM (fmap getFixBinds . runJustParseExpr cc)
          let fixDiffs = map (concatMap ppDiff . snd . applyFixes mod) fixes
          fixDiffs @?= expected,
      localOption (mkTimeout 30_000_000) $
        testCase "Repair BrokenModule finds correct target" $ do
          let cc =
                CompConf
                  { hole_lvl = 0,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude"]
                  }
              toFix = "tests/BrokenModule.hs"
          (_, _, [RProb {..}]) <- moduleToProb cc toFix Nothing
          r_target @?= "broken",
      localOption (mkTimeout 30_000_000) $
        testCase "Repair BrokenGCD" $ do
          let cc =
                CompConf
                  { hole_lvl = 0,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude"]
                  }
              toFix = "tests/BrokenGCD.hs"
              repair_target = Just "gcd'"
              expected =
                map
                  unlines
                  [ [ "---tests/BrokenGCD.hs",
                      "+++tests/BrokenGCD.hs",
                      "@@ -17,3 +17,3 @@ gcd' 0 b = gcd' 0 b",
                      "-gcd' 0 b = gcd' 0 b",
                      "+gcd' 0 b = b",
                      " gcd' a b | b == 0 = a",
                      " gcd' a b = if (a > b) then gcd' (a - b) b else gcd' a (b - a)"
                    ]
                  ]
          (cc', mod, [rp]) <- moduleToProb cc toFix repair_target
          fixes <- (translate cc' rp >>= repair cc') >>= mapM (fmap getFixBinds . runJustParseExpr cc)
          let fixDiffs = map (concatMap ppDiff . snd . applyFixes mod) fixes
          fixDiffs @?= expected,
      localOption (mkTimeout 30_000_000) $
        testCase "Repair MagicConstant" $ do
          let cc =
                CompConf
                  { hole_lvl = 0,
                    packages = ["base", "process", "QuickCheck"],
                    importStmts = ["import Prelude"]
                  }
              toFix = "tests/MagicConstant.hs"
              repair_target = Nothing
              expected =
                map
                  unlines
                  [ [ "---tests/MagicConstant.hs",
                      "+++tests/MagicConstant.hs",
                      "@@ -7,1 +7,1 @@ theAnswer = 17",
                      "-theAnswer = 17",
                      "+theAnswer = 42"
                    ]
                  ]

          (cc', mod, [rp]) <- moduleToProb cc toFix repair_target
          fixes <- (translate cc' rp >>= repair cc') >>= mapM (fmap getFixBinds . runJustParseExpr cc)
          let fixDiffs = map (concatMap ppDiff . snd . applyFixes mod) fixes
          fixDiffs @?= expected
    ]

main = defaultMain tests
