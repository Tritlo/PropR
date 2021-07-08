{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Endemic.Search.Genetic.GenMonad where

import Control.Monad(when, replicateM)
import System.Random
import Data.Maybe
import Data.List(sortBy,delete, groupBy, sortOn, partition)
import Data.Time.Clock

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Endemic.Types (EFix, EProblem (EProb, e_prog, e_ty), CompileConfig, ExprFitCand)
import GHC (SrcSpan, HsExpr, GhcPs, isSubspanOf)
import qualified Data.Map as Map
import Data.Function (on)
import GhcPlugins (HasCallStack,ppr, showSDocUnsafe, liftIO, getOrigNameCache, CompilerInfo (UnknownCC), Outputable(..))

import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State.Lazy as ST
import Control.Monad.Trans.Class (lift)
import Endemic.Repair (repairAttempt)
import Endemic.Util
import Endemic.Traversals (replaceExpr)
import Endemic.Eval (checkFixes)
import qualified Data.Set as Set

import Control.Monad.Par.IO (IVar, ParIO, runParIO)
import Control.Monad.Par.Combinator (parMapM)
import Control.DeepSeq (NFData(..), rwhnf)
import Control.Concurrent.Async

import Endemic.Search.Genetic.Types
import Endemic.Search.Genetic.Configuration
import Endemic.Search.Genetic.Utils

-- ===========                                    ==============
-- ===             EFix Chromosome implementation            ===
-- ===========                                    ==============

-- | This instance is required to implement "EQ" for Efixes. and it's associated GenMonad
-- The Efixes are a Map SrcSpan (HsExpr GhcPs), where the SrcSpan (location in the program) already has a suitable
-- EQ instance. For our purposes, it is hence fine to just compare the "toString" of both Expressions.
-- We do not recommend using this as an implementation for other programs.
instance Eq (HsExpr GhcPs) where
    (==) = (==) `on` showSDocUnsafe . ppr

instance Ord (HsExpr GhcPs) where
    compare = compare `on` showSDocUnsafe . ppr

instance NFData (HsExpr GhcPs) where
    rnf expr = seq (showSDocUnsafe (ppr expr)) ()

splitGenList :: StdGen -> [StdGen]
splitGenList g = g':splitGenList g''
    where (g',g'') = System.Random.split g

instance Chromosome EFix where
    crossover (f1,f2) = collectStats $ efixCrossover f1 f2

    mutate e1 = head <$> mutateMany [e1]
    mutateMany exprs = collectStats $ do
        GConf {..} <- R.ask
        gen <- getGen
        flips <- mapM (\e -> (e,) . (not (Map.null e) &&) <$> tossCoin dropRate) exprs
        let (to_drop_w_inds, to_mutate_w_inds) = partition (snd . snd) $ zip [0..] flips
            (to_mutate_inds, to_mutate) = map fst <$> unzip to_mutate_w_inds
            (to_drop_inds  , to_drop)   = map fst <$> unzip to_drop_w_inds
            drop :: EFix -> StdGen -> EFix
            drop e g = let ks = Map.keys e
                           Just (key_to_drop, _) = pickElementUniform (Map.keys e) g
                        in Map.delete key_to_drop e
            (gen':gens) = splitGenList gen
            dropped = zipWith drop to_drop gens

            EProb{..} = progProblem
            prog_at_ty = progAtTy e_prog e_ty
            cc = compConf
            prob = progProblem
            ecfs = Just exprFitCands
            n_progs = map (`replaceExpr` prog_at_ty) to_mutate
            (gen'':gens') = splitGenList gen'
            selection ((p,pFixes),gen)
                = case pickElementUniform pFixes gen of
                    -- No possible fix, meaning we don't have any locations
                    -- to change... which means we've already solved it!
                    -- TODO: is this always the case when we get Nothing
                    -- here?
                    Nothing -> return p
                    -- Fix res here is:
                    -- + Right True if all the properties are correct (perfect fitnesss!)
                    -- + Right False if the program doesn't terminate (worst fitness)..
                    --    Blacklist this fix?
                    -- + Left [Bool] if it's somewhere in between.
                    Just ((fix, fix_res), _) -> do
                       let mf = mergeFixes fix p
                       updateCache (mf, basicFitness mf fix_res)
                       return mf
            mapGen = if useParallelMap then mapConcurrently else mapM
        possibleFixes <- collectStats $
                if null n_progs then return []
                else liftIO $ mapGen (\p -> repairAttempt cc prob {e_prog = p} ecfs) n_progs
        mutated <- mapM selection (zip (zip to_mutate possibleFixes) gens')
        putGen gen''
        return $ map snd $ sortOn fst $ zip to_mutate_inds mutated ++ zip to_drop_inds dropped


    fitness e1 = head <$> fitnessMany [e1]
    fitnessMany exprs = collectStats $ do
            fc <- getCache
            let lookups :: [(EFix, Maybe Double)]
                lookups = map (\e -> (e,) $ fc Map.!? e) exprs
                (done_w_inds, to_compute_w_inds) = partition (isJust . snd . snd) $ zip [0..] lookups
                (to_compute_inds, to_compute) = map fst <$> unzip to_compute_w_inds
                (done_inds, done) = map (\(d, Just r) -> (d,r)) <$> unzip done_w_inds
            if null to_compute
            then return $ map snd done
            else do GConf{..} <- R.ask
                    let EProb{..} = progProblem
                        prog_at_ty = progAtTy e_prog e_ty
                        cc = compConf
                        n_progs = map (`replaceExpr` prog_at_ty) to_compute
                    res <- zipWith (\e f -> (e, basicFitness e f)) to_compute <$>
                                    liftIO (checkFixes cc progProblem n_progs)
                    putCache (Map.fromList res `Map.union` fc)
                    return $ map (snd . snd) $ sortOn fst $
                             zip to_compute_inds res ++ zip done_inds done


    initialPopulation 0 = return [] -- We don't want to do any work if there's no work to do.
    initialPopulation n = collectStats $
         do GConf{..} <- R.ask
            let EProb{..} = progProblem
                cc = compConf
                prob = progProblem
                ecfs = Just exprFitCands
            possibleFixes <- liftIO $ repairAttempt cc prob ecfs
            replicateM n $ do
               gen <- getGen
               case pickElementUniform possibleFixes gen of
                  Nothing -> error "WASN'T BROKEN??"
                  -- Fix res here is:
                  -- + Right True if all the properties are correct (perfect fitnesss!)
                  -- + Right False if the program doesn't terminate (worst fitness)..
                  --    Blacklist this fix?
                  -- + Left [Bool] if it's somewhere in between.
                  Just ((fix, fix_res), gen''') -> do
                     updateCache (fix, basicFitness fix fix_res)
                     putGen gen'''
                     return fix


-- | Calculates the fitness of an EFix by checking it's FixResults (=The Results of the property-tests).
-- It is intended to be cached using the Fitness Cache in the GenMonad.
basicFitness :: EFix -> Either [Bool] Bool -> Double
basicFitness mf fix_res =
     case fix_res of
            Left bools -> 1 - fitness_func (mf, bools) / fromIntegral (length bools)
            Right True -> 0 -- Perfect fitness
            Right False -> 1 -- The worst
    where fitness_func = fromIntegral . length . filter id . snd

-- | A bit more sophisticated crossover for efixes.
-- The Efixes are transformed to a list and for each chromosome a crossover point is selected.
-- Then the Efixes are re-combined by their genes according to the selected crossover point.
efixCrossover :: EFix -> EFix -> GenMonad (EFix,EFix)
efixCrossover a b = do
    GConf{..} <- R.ask
    gen <- getGen
    let
        (aGenotypes,bGenotypes) = (Map.toList a,Map.toList b)
        (crossedAs,crossedBs,gen') = crossoverLists gen aGenotypes bGenotypes
    putGen gen'
    return (Map.fromList crossedAs, Map.fromList crossedBs)
    where
        -- | Merges two lists of expressions,
        -- with the exception that it discards expressions if the position overlaps
        mf' :: [(SrcSpan, HsExpr GhcPs)] -> [(SrcSpan, HsExpr GhcPs)] -> [(SrcSpan, HsExpr GhcPs)]
        mf' [] xs = xs
        mf' xs [] = xs
        mf' (x : xs) ys = x : mf' xs (filter (not . isSubspanOf (fst x) . fst) ys)
        -- | Makes the (randomized) pairing of the given lists for later merging
        -- TODO: Doublecheck if this can create a merge from as ++ [] depending on the crossoverpoint logic,
        -- Or if it needs to be: uniformR (1, length as - 1) gen
        crossoverLists :: (RandomGen g) => g ->
            [(SrcSpan, HsExpr GhcPs)]
            -> [(SrcSpan, HsExpr GhcPs)]
            -> ([(SrcSpan, HsExpr GhcPs)],[(SrcSpan, HsExpr GhcPs)],g)
        -- For empty chromosomes, there is no crossover possible
        crossoverLists gen [] [] = ([],[],gen)
        -- For single-gene chromosomes, there is no crossover possible
        crossoverLists gen [a] [b] = ([a],[b],gen)
        crossoverLists gen as bs =
            let
             (crossoverPointA, gen') = uniformR (1, length as) gen
             (crossoverPointB, gen'') = uniformR (1, length bs) gen'
             (part1A,part2A) = splitAt crossoverPointA as
             (part1B,part2B) = splitAt crossoverPointB bs
            in (mf' part1A part2B, mf' part1B part2A,gen'')

-- | Merging fix-candidates is mostly applying the list of changes in order.
--   The only addressed special case is to discard the next change,
--   if the next change is also used at the same place in the second fix.
mergeFixes :: EFix -> EFix -> EFix
mergeFixes f1 f2 = Map.fromList $ mf' (Map.toList f1) (Map.toList f2)
  where
    mf' [] xs = xs
    mf' xs [] = xs
    mf' (x : xs) ys = x : mf' xs (filter (not . isSubspanOf (fst x) . fst) ys)

-- | This method tries to reduce a Fix to a smaller, but yet still correct Fix.
-- To achieve this, the Parts of a Fix are tried to be removed and the fitness function is re-run.
-- If the reduced Fix still has a perfect fitness, it is returned in a list of potential fixes.
-- The output list is sorted by length of the fixes, the head is the smallest found fix.
minimizeFix :: EFix -> GenMonad [EFix]
minimizeFix bigFix = do
    fitnesses <- fitnessMany candidateFixes
    let
        fitnessedCandidates = zip fitnesses candidateFixes
        reducedWinners = filter (\(f,c)->f==0) fitnessedCandidates
        reducedWinners' = map snd reducedWinners
    return reducedWinners'
    where
        candidates = powerset $ Map.toList bigFix
        candidates' = sortBy (\c1 c2-> compare (length c1) (length c2)) candidates
        -- TODO: If I do fitness, are they still ... sorted?
        candidateFixes = map Map.fromList candidates'

runGenMonad :: GeneticConfiguration -> Int -> GenMonad a -> IO a
runGenMonad conf seed action = do
         let withConf = R.runReaderT action conf
             withGen = ST.runStateT withConf (mkStdGen seed)
         ((result,_ :: StdGen), _ :: FitnessCache) <- ST.runStateT withGen Map.empty
         return result

-- A version of RGM that takes a generator and cache instead of a seed.
runGenMonad' :: GeneticConfiguration
             -> StdGen -> FitnessCache -> GenMonad a -> IO ((a, StdGen), FitnessCache)
runGenMonad' conf gen fc action = do
         let withConf = R.runReaderT action conf
             withGen = ST.runStateT withConf gen
         ST.runStateT withGen fc

-- Some getters and setters for the monad
getGen :: GenMonad StdGen
getGen = lift ST.get

putGen :: StdGen -> GenMonad ()
putGen = lift . ST.put

getCache :: GenMonad FitnessCache
getCache = lift (lift ST.get)

putCache :: FitnessCache -> GenMonad ()
putCache = lift . lift . ST.put

updateCache :: (EFix, Double) -> GenMonad ()
updateCache (k,v) = getCache >>= putCache . Map.insert k v

-- | Helper to clearer use "randomR" of the RandomPackage for our GenMonad.
getRandomDouble ::
    Double                  -- ^ lower bound, included in the possible values
    -> Double               -- ^ upper bound, included in the possible values
    -> GenMonad Double      -- ^ a values chosen from a uniform distribution (low,high), and the updated GenMonad with the new Generator
getRandomDouble lo hi =
     do gen <- getGen
        let (res, new_gen) = randomR (lo, hi) gen
        putGen new_gen
        return res

tossCoin :: Double -> GenMonad Bool
tossCoin rate = do gen <- getGen
                   let (res,gen') = coin rate gen
                   putGen gen'
                   return res