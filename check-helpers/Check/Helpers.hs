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

-- QuickCheck helpers
qcCheckArgs :: Int -> Int -> Args
qcCheckArgs seed ms = stdArgs {chatty = False, replay = Just (mkQCGen seed, seed), maxShrinks = ms}

qcCheckArgsNoMax :: Int -> Args
qcCheckArgsNoMax seed = stdArgs {chatty = False, replay = Just (mkQCGen seed, seed)}

qcSuccess :: Result -> Bool
qcSuccess = isSuccess

qcWRes :: Testable prop => Int -> Args -> prop -> IO Result
qcWRes wit args prop = quickCheckWithResult args (within wit $ prop)

-- TODO: Add seed
qcWithin :: Testable prop => Int -> prop -> Property
qcWithin = within

failureToMaybe :: Result -> Maybe [String]
failureToMaybe Failure {..} = Just failingTestCase
failureToMaybe _ = Nothing

-- Tasty helpers
checkTastyTree :: Int -> TestTree -> IO Bool
checkTastyTree timeout tt =
  catch
    (withArgs [] (fmap (const True) $ defaultMain $ localOption (mkTimeout (fromIntegral timeout)) tt))
    (return . (==) ExitSuccess)
