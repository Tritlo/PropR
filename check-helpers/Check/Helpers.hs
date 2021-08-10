{-# LANGUAGE RecordWildCards #-}

module Check.Helpers
  ( module Check.Helpers,
    module QuickCheck,
  )
where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (catch)
import qualified Data.IntMap as IntMap
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode (ExitSuccess))
import Test.QuickCheck
import qualified Test.QuickCheck as QuickCheck (Property)
import Test.QuickCheck.Random
import Test.Tasty
import Test.Tasty (defaultMain, localOption, mkTimeout)
import qualified Test.Tasty as Tasty
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
checkTastyTree timeout test =
  case tryIngredients [TestReporter [] (\opts tt -> Just reportFun)] mempty with_timeout of
    Just act -> act
    _ -> return False
  where
    with_timeout = localOption (mkTimeout (fromIntegral timeout)) test
    waitUntilDone :: TVar TR.Status -> IO Bool
    waitUntilDone status_var = atomically $ do
      status <- readTVar status_var
      case status of
        TR.Done res -> return $ TR.resultSuccessful res
        _ -> retry

    reportFun :: TR.StatusMap -> IO (TR.Time -> IO Bool)
    -- We completely ignore the parallelism here
    reportFun smap = do
      results <- mapM waitUntilDone $ IntMap.elems smap
      return (\_ -> return $ and results)

unfoldTastyTests :: TestTree -> [TestTree]
unfoldTastyTests = TR.foldTestTree (TR.trivialFold {TR.foldSingle = fs'}) mempty
  where
    fs' opts name test = [TR.PlusTestOptions (opts <>) $ TR.SingleTest name test]

testTreeNthTest :: Int -> TestTree -> TestTree
testTreeNthTest n tree = (unfoldTastyTests tree) !! n