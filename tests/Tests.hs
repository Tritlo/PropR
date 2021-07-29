{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Arrow
import Data.Bits (finiteBitSize)
import Data.Default
import Data.Dynamic (Dynamic, fromDynamic)
import Data.List (find, sort)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Data.Tree
import Data.Tuple (swap)
import Endemic.Configuration
import Endemic.Diff
import Endemic.Eval
import Endemic.Repair
import Endemic.Traversals
import Endemic.Types
import Endemic.Util
import GHC (GhcPs, LHsExpr, tm_parsed_module)
import GhcPlugins (GenLocated (L), getLoc, ppr, showSDocUnsafe, unLoc)
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

-- Helpers
compileParsedCheck :: HasCallStack => CompileConfig -> EExpr -> IO Dynamic
compileParsedCheck cc expr =
  runGhc' (cc {hole_lvl = 0}) $
    dynCompileParsedExpr `reportOnError` expr

runJustParseExpr :: CompileConfig -> RExpr -> IO (LHsExpr GhcPs)
runJustParseExpr cc str = runGhcWithCleanup $ justParseExpr cc str

-- | Chosen fairly by Random.org
tESTSEED :: Int
tESTSEED = 490_100_041

prop_insertAt :: Eq a => Int -> a -> [a] -> Property
prop_insertAt n a as = abs n < length as ==> insertAt n' a as !! n' == a
  where
    n' = n `mod` length as

utilTests :: TestTree
utilTests =
  testProperties
    "Utils"
    [ ("dropPrefix", property prop_dropsPrefix),
      ("startsWith", property prop_startsWith),
      ("insertAt", property (prop_insertAt @Integer))
    ]

repairTests =
  testGroup
    "Repair"
    [ -- A simple tests to see if we can repair (foldl (-) 0) to (foldl (+) 0)
      -- in a reasonable amount of time (here 10s)
      localOption (mkTimeout 30_000_000) $
        testCase "Basic Repair `foldl (-) 0`" $ do
          let cc =
                def
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
          setSeedGenSeed tESTSEED
          tp@EProb {..} <- translate cc rp
          fixes <- repair cc def tp
          let fixProgs = map (eProgToEProgFix . applyFixToEProg e_prog) fixes
          expected `elem` concatMap (map (trim . showUnsafe)) fixProgs @? "Expected repair not found in fixes",
      localOption (mkTimeout 20_000_000) $
        testCase "GetExprCands finds important candidates" $ do
          let wrong_prog =
                unlines
                  [ "let { gcd' 0 b = gcd' 0 b",
                    "    ; gcd' a b | b == 0 = a",
                    "    ; gcd' a b = if (a > b) then gcd' (a-b) b else gcd' a (b-a)",
                    "    ; a_constant_string = \"hello, world!\"}",
                    "     in gcd'"
                  ]
              expected =
                [ "EFC {\"hello, world!\"}",
                  "EFC {0}",
                  "EFC {gcd' 0 b}",
                  "EFC {gcd' 0}",
                  "EFC {0}",
                  "EFC {0}",
                  "EFC {b == 0}",
                  "EFC {0}",
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
          expr_cands <- runJustParseExpr def wrong_prog >>= (runGhc' def . getExprFitCands . Left)
          map showUnsafe expr_cands @?= expected,
      localOption (mkTimeout 20_000_000) $
        testCase "Repair `gcd'` with gcd" $ do
          let props =
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
          setSeedGenSeed tESTSEED
          fixes <- map (trim . showUnsafe) <$> (translate def rp >>= repair def def)
          not (null fixes) @? "No fix found"
    ]

failingPropsTests =
  testGroup
    "Failing props"
    [ localOption (mkTimeout 15_000_000) $
        testCase "Failing props for gcd" $ do
          let props :: [String]
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
          setSeedGenSeed tESTSEED
          tp <- translate def rp
          failed_props <- failingProps def def tp
          -- Only the first prop should be failing (due to an infinite loop)
          map showUnsafe failed_props @?= [head props],
      localOption (mkTimeout 10_000_000) $
        testCase "Only one failing prop" $ do
          let cc =
                def
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
          setSeedGenSeed tESTSEED
          tp <- translate cc rp
          failed_props <- failingProps def cc tp
          map showUnsafe failed_props @?= props,
      localOption (mkTimeout 10_000_000) $
        testCase "Two failing TastyProps" $ do
          Just desc@ProbDesc {..} <- describeProblem def "tests/cases/TastyTwoFix.hs"
          failed_props <- failingProps repConf compConf progProblem
          length failed_props @?= 2
    ]

counterExampleTests =
  testGroup
    "Counter Examples"
    [ localOption (mkTimeout 10_000_000) $
        testCase "Only one counter example" $ do
          let cc =
                def
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
          setSeedGenSeed tESTSEED
          tp <- translate cc rp
          [failed_prop] <- failingProps def cc tp
          Just [counter_example] <- propCounterExample def cc tp failed_prop
          let expr = "(foldl (-) 0) " ++ counter_example ++ " == sum " ++ counter_example
          res <- runJustParseExpr cc expr >>= compileParsedCheck cc
          case fromDynamic @Bool res of
            Just v -> not v @? "Counter Example is not a counter example!"
            Nothing -> error "Incorrect type!!",
      localOption (mkTimeout 10_000_000) $
        testCase "Multiple examples" $ do
          let cc =
                def
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
          setSeedGenSeed tESTSEED
          tp <- translate cc rp
          [failed_prop] <- failingProps def cc tp
          Just counter_example_args <- propCounterExample def cc tp failed_prop
          let arg_str = unwords counter_example_args
              expr = "(-) " ++ arg_str ++ " == (+) " ++ arg_str
          res <- runJustParseExpr cc expr >>= compileParsedCheck cc
          case fromDynamic @Bool res of
            Just v -> not v @? "Counter Example is not a counter example!"
            Nothing -> error "Incorrect type!!",
      localOption (mkTimeout 15_000_000) $
        testCase "No args loop fail" $ do
          let cc = def
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
          setSeedGenSeed tESTSEED
          tp <- translate cc rp
          [failed_prop] <- failingProps def cc tp
          -- Only the first prop should be failing (due to an infinite loop)
          showUnsafe failed_prop @?= head props
          Just counter_example_args <- propCounterExample def cc tp failed_prop
          null counter_example_args @? "The counter example should not have any arguments!"
    ]

traceTests =
  testGroup
    "Trace tests"
    [ localOption (mkTimeout 10_000_000) $
        testCase "Trace foldl" $ do
          let cc = def
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
          setSeedGenSeed tESTSEED
          tp@EProb {..} <- translate cc rp
          [failed_prop] <- failingProps def cc tp
          Just counter_example <- propCounterExample def cc tp failed_prop
          let eprog_fix = eProgToEProgFix e_prog
          Just [Node {subForest = [tree@Node {rootLabel = (tl, tname)}]}] <-
            traceTarget def cc tp eprog_fix failed_prop counter_example
          expr <- runJustParseExpr cc wrong_prog
          getLoc expr @?= mkInteractive tl
          all ((== 1) . snd) (concatMap snd $ flatten tree) @? "All subexpressions should be touched only once!",
      localOption (mkTimeout 30_000_000) $
        testCase "Trace finds loop" $ do
          let cc = def
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
          setSeedGenSeed tESTSEED
          tp@EProb {..} <- translate cc rp
          [failed_prop] <- failingProps def cc tp
          Just counter_example_args <- propCounterExample def cc tp failed_prop
          -- We generate the trace
          let [(_, e_ty, e_prog')] = e_prog
              prog_at_ty = progAtTy e_prog' e_ty
              eprog_fix = eProgToEProgFix $ applyFixToEProg e_prog mempty
          [tcorrel] <- buildTraceCorrel cc tp eprog_fix
          Just [res] <- traceTarget def cc tp eprog_fix failed_prop counter_example_args
          let eMap = Map.fromList $ map (getLoc &&& showUnsafe) $ flattenExpr prog_at_ty
              chain l = tcorrel Map.!? l >>= (eMap Map.!?)
              trc = map (\(s, r) -> (chain $ mkInteractive s, r, maximum $ map snd r)) $ flatten res
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
          let cc = def
              toFix = "tests/cases/BrokenModule.hs"
              repair_target = Just "broken"
          (cc', _, Just EProb {..}) <- moduleToProb cc toFix repair_target
          -- There are 7 ways to replace parts of the broken function in BrokenModule
          -- with holes:
          let [(_, _, e_prog')] = e_prog
          length (sanctifyExpr e_prog') @?= 7,
      localOption (mkTimeout 1_000_000) $
        testCase "Fill foldl program" $ do
          let cc = def
              toFix = "tests/cases/BrokenModule.hs"
              repair_target = Just "broken"
          (cc', _, Just EProb {..}) <- moduleToProb cc toFix repair_target
          let [(_, _, e_prog')] = e_prog
              (holes, holey) = unzip $ sanctifyExpr e_prog'
              filled = mapMaybe (fillHole undefVar) holey
          length filled @?= 7
          all (uncurry (==)) (zip holes (map fst filled)) @? "All fillings should match holes!"
    ]

mkModuleTest :: Integer -> TestName -> FilePath -> Maybe String -> [[Char]] -> TestTree
mkModuleTest timeout tag toFix repair_target expected =
  localOption (mkTimeout timeout) $
    testCase tag $ do
      setSeedGenSeed tESTSEED
      (cc', mod, mb_prob) <- moduleToProb def toFix repair_target
      case mb_prob of
        Nothing -> [] @?= expected
        Just ExProb {..} -> error "not supported yet!"
        Just tp@EProb {..} -> do
          fixes <- repair cc' def tp
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

moduleTests =
  testGroup
    "Module tests"
    [ localOption (mkTimeout 30_000_000) $
        testCase "Repair BrokenModule finds correct target" $ do
          let toFix = "tests/cases/BrokenModule.hs"
          (_, _, Just EProb {..}) <- moduleToProb def toFix Nothing
          let [(e_target, _, _)] = e_prog
          showUnsafe e_target @?= "broken",
      mkModuleTest 30_000_000 "Repair BrokenModule With Diff" "tests/cases/BrokenModule.hs" (Just "broken") $
        map
          unlines
          [ [ "diff --git a/tests/cases/BrokenModule.hs b/tests/cases/BrokenModule.hs",
              "--- a/tests/cases/BrokenModule.hs",
              "+++ b/tests/cases/BrokenModule.hs",
              "@@ -8,1 +8,1 @@ broken = foldl (-) 0",
              "-broken = foldl (-) 0",
              "+broken = sum"
            ],
            [ "diff --git a/tests/cases/BrokenModule.hs b/tests/cases/BrokenModule.hs",
              "--- a/tests/cases/BrokenModule.hs",
              "+++ b/tests/cases/BrokenModule.hs",
              "@@ -8,1 +8,1 @@ broken = foldl (-) 0",
              "-broken = foldl (-) 0",
              "+broken = foldl add 0"
            ],
            [ "diff --git a/tests/cases/BrokenModule.hs b/tests/cases/BrokenModule.hs",
              "--- a/tests/cases/BrokenModule.hs",
              "+++ b/tests/cases/BrokenModule.hs",
              "@@ -8,1 +8,1 @@ broken = foldl (-) 0",
              "-broken = foldl (-) 0",
              "+broken = foldl (+) 0"
            ]
          ],
      mkModuleTest 90_000_000 "Repair BrokenGCD" "tests/cases/BrokenGCD.hs" (Just "gcd'") $
        map
          unlines
          [ [ "diff --git a/tests/cases/BrokenGCD.hs b/tests/cases/BrokenGCD.hs",
              "--- a/tests/cases/BrokenGCD.hs",
              "+++ b/tests/cases/BrokenGCD.hs",
              "@@ -19,3 +19,3 @@ gcd' 0 b = gcd' 0 b",
              "-gcd' 0 b = gcd' 0 b",
              "+gcd' 0 b = b",
              " gcd' a b | b == 0 = a",
              " gcd' a b = if (a > b) then gcd' (a - b) b else gcd' a (b - a)"
            ]
          ],
      mkModuleTest 30_000_000 "Repair MagicConstant" "tests/cases/MagicConstant.hs" Nothing $
        map
          unlines
          [ [ "diff --git a/tests/cases/MagicConstant.hs b/tests/cases/MagicConstant.hs",
              "--- a/tests/cases/MagicConstant.hs",
              "+++ b/tests/cases/MagicConstant.hs",
              "@@ -7,1 +7,1 @@ theAnswer = 17",
              "-theAnswer = 17",
              "+theAnswer = 42"
            ]
          ],
      mkModuleTest 10_000_000 "All props pass" "tests/cases/AllPropsPass.hs" Nothing [],
      mkModuleTest 5_000_000 "No props" "tests/cases/NoProps.hs" Nothing []
    ]

main = defaultMain tests
