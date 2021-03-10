{-# LANGUAGE RecordWildCards, TupleSections #-}
module Synth.Repair where

import GHC

import TcHoleErrors (TypedHole (..), HoleFit(..))
import Constraint (Ct(..), holeOcc)
import GhcPlugins (substTyWith, PluginWithArgs(..), StaticPlugin(..)
                  , occName, OccName(..), fsLit, mkOccNameFS, concatFS
                  , HscEnv(hsc_IC), InteractiveContext(ic_default)
                  , mkVarUnqual, getRdrName, showSDocUnsafe, liftIO)

import Control.Monad (filterM, when)

import Data.Maybe
import GHC.Paths (libdir)

import Synth.Eval
import Synth.Check
import Synth.Util
import Synth.Fill
import Synth.Types
import Synth.Sanctify
import Data.Either
import Data.Dynamic (fromDyn)
import Data.List (sortOn)
import Data.Set (Set)
import Data.Tree (flatten)
import qualified Data.Set as Set

setNoDefaulting :: Ghc ()
setNoDefaulting =
  -- Make sure we don't do too much defaulting by setting `default ()`
  -- Note: I think this only applies to the error we would be generating,
  -- I think if we replace the UnboundVar with a suitable var of the right
  -- it would work... it just makes the in-between output a bit confusing.
  do env <- getSession
     setSession (env {hsc_IC = (hsc_IC env) {ic_default = Just []}})

getHoleFits :: CompileConfig -> LHsExpr GhcPs -> IO [[HoleFit]]
getHoleFits cc expr = runGhc (Just libdir) $ do
   plugRef <- initGhcCtxt cc
   -- Then we can actually run the program!
   setNoDefaulting
   res <- handleSourceError (getHoleFitsFromError plugRef)
                            (compileParsedExpr expr >>= (return . Right))
   return $ case res of
              Left r -> map fst r
              Right _ -> []

getHoley :: CompileConfig -> RExpr -> IO [(SrcSpan, LHsExpr GhcPs)]
getHoley cc str = runGhc (Just libdir) $ exprHoley cc str

exprHoley :: CompileConfig -> RExpr -> Ghc [(SrcSpan, LHsExpr GhcPs)]
exprHoley cc str = sanctifyExpr <$> justParseExpr cc str

justParseExpr :: CompileConfig -> RExpr -> Ghc (LHsExpr GhcPs)
justParseExpr cc str = do
   plugRef <- initGhcCtxt cc
   handleSourceError
     (\err -> printException err >> error "parse failed")
     (parseExpr str)

runJustParseExpr :: CompileConfig -> RExpr -> IO (LHsExpr GhcPs)
runJustParseExpr cc str = runGhc (Just libdir) $ justParseExpr cc str

fillHoleWithFit :: LHsExpr GhcPs -> HoleFit -> Maybe (LHsExpr GhcPs)
fillHoleWithFit expr fit = fillHole expr (HsVar noExtField (L noSrcSpan (toName fit)))
 where toName (RawHoleFit sd) = mkVarUnqual $ fsLit $ showSDocUnsafe sd
       toName (HoleFit {hfId = hfId}) = getRdrName hfId



replacements :: LHsExpr GhcPs -> [[HoleFit]] -> [LHsExpr GhcPs]
replacements e [] = [e]
replacements e (first_hole_fit:rest) =
    (mapMaybe (fillHoleWithFit e) first_hole_fit) >>= (flip replacements rest)


-- Builds a check of a program by parsing the context and the generated
-- expression and replacing the relevant holes.
buildCheck :: (RProblem -> RExpr) -> CompileConfig -> RProblem
           -> IO (CompileConfig, RExpr)
buildCheck bc cc rp@RProb{..} = do
   do let prog_at_ty = "("++ r_prog ++ ") :: " ++ r_ty
      pr_debug prog_at_ty
      parsed <- runJustParseExpr cc prog_at_ty
      holeyContext <- runJustParseExpr cc $ contextLet r_ctxt "_"
      let wContext = fromJust $ fillHole holeyContext $ unLoc parsed
      bcatC <- runJustParseExpr cc $ bc rp{r_prog="_"}
      to_check <- runJustParseExpr cc $ trim $ showUnsafe wContext
      let check = fromJust $ fillHole bcatC $ unLoc to_check
          cc' = (cc {hole_lvl=0, importStmts=(checkImports ++ importStmts cc)})
      return (cc', showUnsafe check)


-- Get a list of strings which represent shrunk arguments to the property that
-- makes it fail.
propCounterExample :: CompileConfig -> RProblem -> RProp -> IO (Maybe [RExpr])
propCounterExample cc rp prop = do
    (cc', check_exp) <- buildCheck (buildCounterExampleExpr prop) cc rp
    exec <- compileCheck cc' check_exp
    res <- fromDyn exec (return Nothing)
    return res


-- Returns the props that fail for the given program
failingProps :: CompileConfig -> RProblem -> IO [RProp]
failingProps _ RProb{r_props=[]} = return []
-- Our method for checking which props fail is restricted to maximum 8 at a time,
-- so if we have more than that, we check the first 8 and then the rest, and
-- so on.
failingProps cc rp@(RProb{r_props=ps}) | length ps > 8 = do
  let (ps1, ps2) = splitAt 8 ps
  p1 <- failingProps cc rp{r_props=ps1}
  p2 <- failingProps cc rp{r_props=ps2}
  return (p1 ++ p2)
failingProps cc rp@RProb{..} = do
      (cc', check) <- buildCheck buildCheckExprAtTy cc rp
      [compiled_check] <- compileChecks cc' [check]
      ran <- runCheck compiled_check
      case ran of
         -- Some of the props are failing:
         Left p -> return $ map fst $ filter (\(p,c) -> not c) $ zip r_props p
         -- None of the props are failing:
         Right True -> return []
         -- One of the props is causing an error/infinite loop, so we need
         -- to check each individually
         Right False ->
            case r_props of
              -- If there's only one failing prop left, that's the one causing
              -- the loop
              [prop] -> return [prop]
              -- Otherwise, we split the props into two sets, and check each
              -- split individually.
              xs -> do let fp :: [String] -> IO [String]
                           fp ps = failingProps cc rp{r_props=ps}
                           ps1, ps2 :: [String]
                           (ps1, ps2) = splitAt (length r_props `div` 2) r_props
                       concat <$> mapM fp [ps1, ps2]


repair :: CompileConfig -> RProblem -> IO [RExpr]
repair cc rp@RProb{..} =
   do let prog_at_ty = "("++ r_prog ++ ") :: " ++ r_ty
      pr_debug prog_at_ty
      holey_exprs <- getHoley cc prog_at_ty
      pr_debug $ showUnsafe holey_exprs


      -- We can use the failing_props and the counter_examples to filter
      -- out locations that we know won't matter.
      failing_props <- failingProps cc (rp {r_prog=prog_at_ty})
      counter_examples <- mapM (propCounterExample cc rp) failing_props
      let hasCE (p, Just ce) = Just (p, ce)
          hasCE _ = Nothing
          -- We find the ones with counter-examples and pick the shortest one
          ps_w_ce =
             sortOn (length . snd ) $ mapMaybe hasCE $ zip failing_props counter_examples
      holey_exprs <- case ps_w_ce of
         (p,ce):_ ->
             -- We run the trace to find which expressions are touched in the
             -- counter example
             do trc <- traceTarget cc prog_at_ty p ce
                case trc of
                   -- We then remove suggested holes that are unlikely to help
                   -- (naively for now in the sense that we remove only holes
                   -- which did not get evaluated at all, so they are definitely
                   -- not going to matter).
                   Just res -> do
                      let only_max (src, r) = (mkInteractive src, maximum $ map snd r)
                          invokes = map only_max $ flatten res
                          non_zero = filter (\(src,n) -> n > 0) invokes
                          non_zero_src = Set.fromList $ map fst non_zero
                          non_zero_holes = filter (\(l,e) -> l `Set.member` non_zero_src) holey_exprs
                      pr_debug "Invokes:"
                      pr_debug $ showUnsafe invokes
                      pr_debug "Non-zero holes:"
                      pr_debug $ showUnsafe non_zero_holes
                      return non_zero_holes
                   _ -> return holey_exprs
         _ -> return holey_exprs

      -- We add the context by replacing a hole in a let.
      holeyContext <- runJustParseExpr cc $ contextLet r_ctxt "_"
      let addContext = fromJust . fillHole holeyContext . unLoc
      fits <- mapM (\(_,e) -> (e,) <$> (getHoleFits cc $ addContext e)) holey_exprs
      let repls = fits >>= (uncurry replacements)

      -- We do it properly
      bcatC <- runJustParseExpr cc $ buildCheckExprAtTy rp{r_prog="_"}
      to_checks <- mapM (runJustParseExpr cc . trim . showUnsafe) repls
      pr_debug $ showUnsafe bcatC
      pr_debug $ showUnsafe to_checks
      let checks = map ( fromJust . fillHole bcatC . unLoc) to_checks
      pr_debug  "Fix candidates:"
      mapM (pr_debug . showUnsafe) checks
      pr_debug "Those were all of them!"
      let cc' = (cc {hole_lvl=0, importStmts=(checkImports ++ importStmts cc)})
      compiled_checks <- zip repls <$> compileChecks cc' (map showUnsafe checks)
      ran <- mapM (\(f,c) -> runCheck c >>= return . (f,)) compiled_checks
      let res2 = map fst $ filter (\(f,r) -> r == Right True) ran
      return $ map showUnsafe res2
