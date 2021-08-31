{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : PropR.Search.Genetic
-- Description : Holds the (revamped) Genetic Algorithm Parts of PropR
-- License     : MIT
-- Stability   : experimental
-- Portability : POSIX
--
-- This module holds the (reworked) genetic Algorithm parts of the PropR Library.
-- The algorithms are more advanced and sophisticated than the initial implementation.
--
-- The primary building brick is an EFix (See "PropR.Types") that resembles a set of
-- changes done to the Code. The goodness of a certain fix is expressed by the
-- failing and succeeding properties, which are a list of boolean values (true for passing properties, false for failing).
--
-- The methods often require a RandomGen for the random parts. A RandomGen is, for non-haskellers, a random number provider.
-- It is expressed in Haskell as an infinite list of next-random-values.
-- We expect that the RandomGen is generated e.g. in the Main Method from a Seed and passed here.
-- See: https://hackage.haskell.org/package/random-1.2.0/docs/System-Random.html
--
-- **Prefixes**
--
-- - tX is an X related to Tournament Behaviour (e.g. tSize = TournamentSize)
-- - iX is an X related to Island Behaviour (e.g. iConf = IslandConfiguration)
-- - a "pop" is short for population
-- - a "gen" is short for generator, a StdGen that helps to provide random elements
--
-- **GenMonad**
--
-- We happened to come accross some challenges nicely designing this library, in particular
-- we needed some re-occurring parts in nearly all functions.
-- This is why we decided to declare the "GenMonad", that holds
--
-- - the configuration
-- - a random number provider
-- - a cache for the fitness function
-- - IO (as the search logs and takes times)
--
-- **Genetic - Naming**
-- A Chromosome is made up by Genotypes, which are the building bricks of changes/diffs.
-- In our Context, a Genotype is a set of diffs we apply to the Code resembled by an pair of (SourceSpan,Expression),
-- and the Chromosome is a EFix (A map of those).
-- A Phenotype is the "physical implementation" of a Chromosome, in our context that is the program with all the patches applied.
-- That is, our EFix turns from its Genotype to a Phenotype once it is run against the properties.
-- The final representation of Solutions provided by "PropR.Diff" is also a (different) Phenotype.
--
-- **Island Evolution**
-- We also introduce a parallelized genetic algorithm called "Island Evolution".
-- In concept, there are n Island with a separate population. Every x generations, a few
-- species migrate from one Island to another.
-- This should help to "breed" one partial-solution per Island, with the migration helping to bring partial solutions together.
-- This is particularly interesting, as maybe fixes for a program need to originate from two places or multiple changes.
-- In the described paper, the species migrate ring-wise and the best species are being copied,
-- while the worst on the receiving island are simply discarded in favor of the best from the sending island.
-- Further Reading:
-- https://neo.lcc.uma.es/Articles/WRH98.pdf
--
-- **Open Questions // Points**
--
--     - Timeouts are not actually interrupting - the are checked after a generation.
--       That can lead to a heavy plus above the specified timeout, e.g. by big populations on Islands.
--       Can we do this better?
--     - This file is growing quite big, we could consider splitting it up in "Genetic" and "EfixGeneticImplementation" or something like that.
--       Similarly, we could maybe move some of the helpers out.
module PropR.Search.Genetic
  ( runGenMonad,
    geneticSearch,
    geneticSearchPlusPostprocessing,
    module PropR.Search.Genetic.Types,
    module PropR.Search.Genetic.Configuration,
    module PropR.Search.Genetic,
  )
where

import PropR.Configuration.Configure (newSeed)
import PropR.Search.Class
import PropR.Search.Genetic.Configuration
import PropR.Search.Genetic.GenMonad
import PropR.Search.Genetic.Search
import PropR.Search.Genetic.Types

instance Search GeneticConfiguration where
  runRepair conf desc = do
    seed <- newSeed
    runGenMonad conf desc seed geneticSearchPlusPostprocessing
