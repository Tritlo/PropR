{-# LANGUAGE RecordWildCards, TupleSections #-}
module Synth.Repair where

import GHC

import TcHoleErrors (TypedHole (..), HoleFit(..))
import Constraint (Ct(..), holeOcc)
import GhcPlugins (substTyWith, PluginWithArgs(..), StaticPlugin(..)
                  , occName, OccName(..), fsLit, mkOccNameFS, concatFS
                  , HscEnv(hsc_IC), InteractiveContext(ic_default)
                  , mkVarUnqual, getRdrName, showSDocUnsafe)

import Control.Monad (filterM, when)

import Data.Maybe
import GHC.Paths (libdir)

import Synth.Eval
import Synth.Check
import Synth.Util
import Synth.Fill
import Synth.Types
import Data.Either
import Data.Dynamic (fromDyn)

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

getHoley :: CompileConfig -> RExpr -> IO [LHsExpr GhcPs]
getHoley cc str = runGhc (Just libdir) $ exprHoley cc str


exprHoley :: CompileConfig -> RExpr -> Ghc [LHsExpr GhcPs]
exprHoley cc str = makeHoley <$> justParseExpr cc str

justParseExpr :: CompileConfig -> RExpr -> Ghc (LHsExpr GhcPs)
justParseExpr cc str = do
   plugRef <- initGhcCtxt cc
   handleSourceError
     (\err -> printException err >> error "parse failed")
     (parseExpr str)

runJustParseExpr :: CompileConfig -> RExpr -> IO (LHsExpr GhcPs)
runJustParseExpr cc str = runGhc (Just libdir) $ justParseExpr cc str

type Rewrite = LHsExpr GhcPs -> [LHsExpr GhcPs]
type Match = LHsExpr GhcPs -> Bool

-- All possible replacement of one variable with a hole
makeHoley :: Rewrite
makeHoley (L loc (HsApp x l r)) = rl ++ rr
  where rl = map (\e -> L loc (HsApp x e r)) $ makeHoley l
        rr = map (\e -> L loc (HsApp x l e)) $ makeHoley r
makeHoley (L loc (HsVar x (L _ v))) =
    [(L loc (HsUnboundVar x (TrueExprHole name)))]
  where (ns,fs) = (occNameSpace (occName v), occNameFS (occName v))
        name = mkOccNameFS ns (concatFS $ (fsLit "_"):[fs])

makeHoley (L loc (HsPar x l)) =
    map (L loc . HsPar x) $ makeHoley l
makeHoley (L loc (ExprWithTySig x l t)) =
    map (L loc . flip (ExprWithTySig x) t) $ makeHoley l
makeHoley (L loc (HsLet x b e)) =
    fmap (L loc . HsLet x b) $ makeHoley e
makeHoley e = []


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
buildCheck :: ([RProp] -> RContext -> RType -> RExpr -> RExpr)
           -> CompileConfig -> [RProp] -> RContext -> RType -> RExpr
           -> IO (CompileConfig, RExpr)
buildCheck bc cc props context ty wrong_prog = do
   do let prog_at_ty = "("++ wrong_prog ++ ") :: " ++ ty
      pr_debug prog_at_ty
      parsed <- runJustParseExpr cc prog_at_ty
      holeyContext <- runJustParseExpr cc $ contextLet context "_"
      let wContext = fromJust $ fillHole holeyContext $ unLoc parsed
      bcatC <- runJustParseExpr cc $ bc props context ty "_"
      to_check <- runJustParseExpr cc $ trim $ showUnsafe wContext
      let check = fromJust $ fillHole bcatC $ unLoc to_check
          cc' = (cc {hole_lvl=0, importStmts=(qcImport:importStmts cc)})
      return (cc', showUnsafe check)


-- Get a list of strings which represent shrunk arguments to the property that
-- makes it fail.
propCounterExample :: CompileConfig -> RContext -> RType
                   -> RExpr -> RProp -> IO (Maybe [RExpr])
propCounterExample cc ctxt ty expr prop = do
    (cc', check_exp) <- buildCheck buildCounterExampleExpr cc [prop] ctxt ty expr
    exec <- compileCheck cc' check_exp
    res <- fromDyn exec (return Nothing)
    return res


-- Returns the props that fail for the given program
failingProps :: CompileConfig -> [RProp] -> RContext -> RType -> RExpr -> IO [RProp]
failingProps _ [] _ _ _ = return []
-- Our method for checking which props fail is restricted to maximum 8 at a time,
-- so if we have more than that, we check the first 8 and then the rest, and
-- so on.
failingProps cc ps ctxt ty wp | length ps > 8 = do
  let (ps1, ps2) = splitAt 64 ps
  p1 <- failingProps cc ps1 ctxt ty wp
  p2 <- failingProps cc ps2 ctxt ty wp
  return (p1 ++ p2)
failingProps cc props context ty wrong_prog = do
      (cc', check) <- buildCheck buildCheckExprAtTy cc props context ty wrong_prog
      [compiled_check] <- compileChecks cc' [check]
      ran <- runCheck compiled_check
      case ran of
         -- Some of the props are failing:
         Left p -> return $ map fst $ filter (\(p,c) -> not c) $ zip props p
         -- None of the props are failing:
         Right True -> return []
         -- One of the props is causing an error/infinite loop, so we need
         -- to check each individually
         Right False ->
            case props of
              -- If there's only one failing prop left, that's the one causing
              -- the loop
              [prop] -> return [prop]
              -- Otherwise, we split the props into two sets, and check each
              -- split individually.
              xs -> do let fp :: [String] -> IO [String]
                           fp ps = failingProps cc ps context ty wrong_prog
                           ps1, ps2 :: [String]
                           (ps1, ps2) = splitAt (length props `div` 2) props
                       concat <$> mapM fp [ps1, ps2]


repair :: CompileConfig -> [RProp] -> RContext -> RType -> RExpr -> IO [RProp]
repair cc props context ty wrong_prog =
   do let prog_at_ty = "("++ wrong_prog ++ ") :: " ++ ty
      pr_debug prog_at_ty
      res <- getHoley cc prog_at_ty
      pr_debug $ showUnsafe res
      -- We add the context by replacing a hole in a let.
      holeyContext <- runJustParseExpr cc $ contextLet context "_"
      let addContext = fromJust . fillHole holeyContext . unLoc
      fits <- mapM (\e -> (e,) <$> (getHoleFits cc $ addContext e)) res
      let repls = fits >>= (uncurry replacements)

      -- We do it properly
      bcatC <- runJustParseExpr cc $ buildCheckExprAtTy props context ty "_"
      to_checks <- mapM (runJustParseExpr cc . trim . showUnsafe) repls
      pr_debug $ showUnsafe bcatC
      pr_debug $ showUnsafe to_checks
      let checks = map ( fromJust . fillHole bcatC . unLoc) to_checks
      pr_debug  "Fix candidates:"
      mapM (pr_debug . showUnsafe) checks
      let cc' = (cc {hole_lvl=0, importStmts=(qcImport:importStmts cc)})
      compiled_checks <- zip repls <$> compileChecks cc' (map showUnsafe checks)
      ran <- mapM (\(f,c) -> runCheck c >>= return . (f,)) compiled_checks
      let res2 = map fst $ filter (\(f,r) -> r == Right True) ran
      return $ map showUnsafe res2