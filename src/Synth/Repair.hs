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

getHoley :: CompileConfig -> String -> IO [LHsExpr GhcPs]
getHoley cc str = runGhc (Just libdir) $ exprHoley cc str


exprHoley :: CompileConfig -> String -> Ghc [LHsExpr GhcPs]
exprHoley cc str = makeHoley <$> justParseExpr cc str

justParseExpr :: CompileConfig -> String -> Ghc (LHsExpr GhcPs)
justParseExpr cc str = do
   plugRef <- initGhcCtxt cc
   handleSourceError
     (\err -> printException err >> error "parse failed")
     (parseExpr str)

runJustParseExpr :: CompileConfig -> String -> IO (LHsExpr GhcPs)
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
makeHoley e = []


fillHoleWithFit :: LHsExpr GhcPs -> HoleFit -> Maybe (LHsExpr GhcPs)
fillHoleWithFit expr fit = fillHole expr (HsVar noExtField (L noSrcSpan (toName fit)))
 where toName (RawHoleFit sd) = mkVarUnqual $ fsLit $ showSDocUnsafe sd
       toName (HoleFit {hfId = hfId}) = getRdrName hfId


-- Fill the first hole in the expression.
fillHole :: LHsExpr GhcPs -> HsExpr GhcPs -> Maybe (LHsExpr GhcPs)
fillHole (L loc (HsApp x l r)) fit
    = case fillHole l fit of
        Just res -> Just (L loc (HsApp x res r))
        Nothing -> case fillHole r fit of
                        Just res -> Just (L loc (HsApp x l res))
                        Nothing -> Nothing
fillHole (L loc (HsUnboundVar x _)) fit = Just (L loc fit)
fillHole (L loc (HsPar x l)) fit =
    fmap (L loc . HsPar x) $ fillHole l fit
fillHole (L loc (ExprWithTySig x l t)) fit =
    fmap (L loc . flip (ExprWithTySig x) t) $ fillHole l fit
fillHole (L loc (HsLet x b e)) fit =
    fmap (L loc . HsLet x b) $ fillHole e fit
fillHole e _ = Nothing

fillHoles :: LHsExpr GhcPs -> [HsExpr GhcPs] -> Maybe (LHsExpr GhcPs)
fillHoles expr [] = Nothing
fillHoles expr (f:fs) = (fillHole expr f) >>= flip fillHoles fs

replacements :: LHsExpr GhcPs -> [[HoleFit]] -> [LHsExpr GhcPs]
replacements e [] = [e]
replacements e (first_hole_fit:rest) =
    (mapMaybe (fillHoleWithFit e) first_hole_fit) >>= (flip replacements rest)


repair :: CompileConfig -> [String] -> [String] -> String -> String -> IO [String]
repair cc props context ty wrong_prog =
   do let prog_at_ty = "("++ wrong_prog ++ ") :: " ++ ty
      res <- getHoley cc prog_at_ty
      -- We add the context by replacing a hole in a let.
      holeyContext <- runJustParseExpr cc $ contextLet context "_"
      let addContext = fromJust . fillHole holeyContext . unLoc

      fits <- mapM (\e -> (e,) <$> (getHoleFits cc $ addContext e)) res
      let repls = fits >>= (uncurry replacements)
          to_check = map (trim . showUnsafe) repls
          bcat = buildCheckExprAtTy props context ty
          checks = map bcat to_check
      pr_debug  "Fix candidates:"
      mapM pr_debug to_check
      let cc' = (cc {hole_lvl=0, importStmts=(qcImport:importStmts cc)})
      compiled_checks <- zip repls <$> compileChecks cc' checks
      res2 <- map fst <$> filterM (\(r,c) -> runCheck c) compiled_checks
      return $ map showUnsafe res2