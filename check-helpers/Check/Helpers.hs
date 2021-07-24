{-# LANGUAGE RecordWildCards #-}

module Check.Helpers where

import Control.Exception (catch)
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode (ExitSuccess))
import Test.QuickCheck
import Test.QuickCheck.Random
import Test.Tasty
import Test.Tasty (defaultMain, localOption, mkTimeout)
import Test.Tasty.Ingredients
import qualified Test.Tasty.Runners as TR

-- QuickCheck helpers
qcCheckArgs :: Args
qcCheckArgs = stdArgs {chatty = False}

qcCheckArgsMax :: Int -> Args
qcCheckArgsMax ms = stdArgs {chatty = False, maxShrinks = ms}

qcCheckArgsSeed :: Int -> Args
qcCheckArgsSeed seed = stdArgs {chatty = False, replay = Just (mkQCGen seed, 0)}

qcCheckArgsMaxSeed :: Int -> Int -> Args
qcCheckArgsMaxSeed seed ms = stdArgs {chatty = False, replay = Just (mkQCGen seed, 0), maxShrinks = ms}

qcSuccess :: Result -> Bool
qcSuccess = isSuccess

qcWRes :: Testable prop => Int -> Args -> prop -> IO Result
qcWRes wit args prop = quickCheckWithResult args (within wit $ prop)

qcWithin :: Testable prop => Int -> prop -> Property
qcWithin = within

failureToMaybe :: Result -> Maybe [String]
failureToMaybe Failure {..} = Just failingTestCase
failureToMaybe _ = Nothing

-- Tasty helpers
checkTastyTree :: Int -> TestTree -> IO Bool
checkTastyTree timeout tt =
  catch
    (withArgs ["-q"] (fmap (const True) $ defaultMain $ localOption (mkTimeout (fromIntegral timeout)) tt))
    (return . (==) ExitSuccess)

unfoldTastyTests :: TestTree -> [TestTree]
unfoldTastyTests = TR.foldTestTree (TR.trivialFold {TR.foldSingle = fs'}) mempty
  where
    fs' opts name test = [TR.PlusTestOptions (opts <>) $ TR.SingleTest name test]

testTreeNthTest :: Int -> TestTree -> TestTree
testTreeNthTest n tree = (unfoldTastyTests tree) !! n