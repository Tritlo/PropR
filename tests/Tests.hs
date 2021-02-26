{-# LANGUAGE NumericUnderscores #-}
module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Synth.Repair (repair)
import Synth.Eval (CompileConfig(..))
import Synth.Util



tests :: TestTree
tests = testGroup "Tests" [utilTests, repairTests]


utilTests :: TestTree
utilTests = testProperties "Utils" [
      ("dropPrefix", property $ prop_dropsPrefix)
    , ("startsWith", property $ prop_startsWith)
    ]
  where prop_dropsPrefix :: String -> String -> Bool
        prop_dropsPrefix st rest = dropPrefix st (st++rest) == rest
        prop_startsWith :: String -> String -> Bool
        prop_startsWith st rest = startsWith st (st ++ rest) == True

repairTests = testGroup "Repair" [
    -- A simple tests to see if we can repair (foldl (-) 0) to (foldl (+) 0)
    -- in a reasonable amount of time (here 10s)
      localOption (mkTimeout 10_000_000) $
        testCase "Repair 'foldl (-) 0'" $ do
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
    ]


main = defaultMain tests
