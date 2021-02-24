module Main where

import Test.Tasty
import Test.Tasty.HUnit

import Synth.Repair (repair)
import Synth.Util (trim)
import Synth.Eval (CompileConfig(..))


tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests" [
    -- A simple tests to see if we can repair (foldl (-) 0) to (foldl (+) 0)
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
