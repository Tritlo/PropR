{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module PropR.Search.Genetic.Utils where

import Data.List (delete)
import PropR.Search.Genetic.Configuration (GeneticConfiguration (..))
import Control.Monad.IO.Class (liftIO)
import GHC.Stack (HasCallStack)
import PropR.Search.Genetic.Types (GenMonad)
import PropR.Types (LogLevel)
import PropR.Util (logStr)
import System.Random (Random (randomR), RandomGen, uniformR)

-- ===========                 ==============
-- ===      "Non Genetic" Helpers         ===
-- ===========                 ==============

-- | Reads the timeOutInMinutes of a configuration and rounds it to the nearest ms
maxTimeInMS :: GeneticConfiguration -> Int
maxTimeInMS conf = round $ 1000 * timeoutInSeconds conf

-- | removes a given pair from a List, e.g.
-- > removePairFromList [2,7,12,5,1] (1,2)
-- > [7,12,5]
-- > removePairFromList [1,2,3,4,5] (5,6)
-- > [1,2,3,4]
-- Used to remove a drafted set from parents from the population for further drafting pairs.
removePairFromList :: (Eq a) => [a] -> (a, a) -> [a]
removePairFromList as (x, y) = [a | a <- as, a /= x, a /= y]

-- | The normal LogSTR is in IO () and cannot be easily used in GenMonad
-- So this is a wrapper to ease the usage given that the GenMonad is completely local
-- in this module.
logStr' :: HasCallStack => LogLevel -> String -> GenMonad ()
logStr' level str = liftIO $ logStr level str

-- ===========                 ==============
-- ===           Random Parts             ===
-- ===========                 ==============

-- | Determines whether an even with chance x happens.
-- A random number between 0 and 1 is created and compared to x,
-- if the drawn number is smaller it returns true, false otherwise.
-- This leads e.g. that (coin 0.5) returns true and false in 50:50
-- While (coin 0.25) returns true and false in 25:75 ratio
coin ::
  (RandomGen g) =>
  -- | The Probabilty of passing, between 0 (never) and 1 (always).
  Double ->
  -- | The Random number provider
  g ->
  -- | Whether or not the event occured
  (Bool, g)
coin 0 gen = (False, gen) -- Shortcut for false, no random used
coin 1 gen = (True, gen) -- Shortcut for true, no random used
coin th gen =
  let (val, gen') = randomR (0, 1) gen
   in (val < th, gen')

-- | This method finds pairs from a given List.
-- It is used for either finding partners to crossover,
-- Or in terms of Island Evolution to find Islands that swap Individuals.
partitionInPairs :: (Eq a, RandomGen g) => [a] -> g -> ([(a, a)], g)
partitionInPairs [] g = ([], g)
partitionInPairs [_] g = ([], g)
partitionInPairs as g =
  let nextPair = pickRandomPair as g
   in case nextPair of
        Nothing -> ([], g)
        Just (pair, g') ->
          let reducedList = removePairFromList as pair
              (as', g'') = partitionInPairs reducedList g'
           in (pair : as', g'')

-- | Returns the same list shuffled.
shuffle :: (RandomGen g, Eq a) => [a] -> g -> ([a], g)
shuffle [] g = ([], g)
shuffle as g =
  let Just (a, g') = pickElementUniform as g
      as' = delete a as
      (as'', g'') = shuffle as' g'
   in (a : as'', g'')

-- | Picks n random elements from u, can give duplicates (intentional behavior)
pickRandomElements :: (RandomGen g, Eq a) => Int -> g -> [a] -> ([a], g)
pickRandomElements 0 g _ = ([], g)
pickRandomElements _ g [] = ([], g)
pickRandomElements n g as =
  let ((x:_), g') = shuffle as g
      (recursiveResults, g'') = pickRandomElements (n - 1) g' as
   in (x : recursiveResults, g'')

-- | Picks a random pair of a given List.
-- The pair is not removed from the List.
-- Must be given a List with an even Number of Elements.
pickRandomPair :: (Eq a, RandomGen g) => [a] -> g -> Maybe ((a, a), g)
pickRandomPair [] _ = Nothing -- not supported!
pickRandomPair [_] _ = Nothing -- not supported!
pickRandomPair as g =
  if even (length as)
    then
      let -- We only get justs, because we have taken care of empty lists beforehand
          Just (elem1, g') = pickElementUniform as g
          as' = delete elem1 as
          Just (elem2, g'') = pickElementUniform as' g'
       in Just ((elem1, elem2), g'')
    else Nothing

-- | Picks a random element from a list, given the list has elements.
-- All elements have the same likeliness to be drawn.
-- Returns Just (element,updatedStdGen) for lists with elements, or Nothing otherwise
pickElementUniform :: (RandomGen g) => [a] -> g -> Maybe (a, g)
pickElementUniform [] _ = Nothing
pickElementUniform xs g =
  let (ind, g') = uniformR (0, length xs - 1) g
   in Just (xs !! ind, g')

deleteMany :: Eq a => [a] -> [a] -> [a]
deleteMany [] xs = xs
deleteMany (d : ds) xs = deleteMany ds (delete d xs)
