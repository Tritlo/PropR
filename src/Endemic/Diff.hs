{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Endemic.Diff
-- Description : This module pretty-prints the output of the repair.
-- License     : MIT
-- Stability   : experimental
--
-- This module displays the changes/diff introduced by the repair in a colourful
-- and pretty way.  It also beforehand doublechecks that the thing to be printed
-- is an actual valid program. This is a pure module.
--
-- Abbreviations:
-- - pp: PrettyPrint
module Endemic.Diff where

import Bag (bagToList)
import Control.Exception (assert)
import Data.Function (on)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Endemic.Configuration (ProblemDescription (..))
import Endemic.Traversals (replaceExpr)
import Endemic.Types
import Endemic.Util (progAtTy, showUnsafe)
import FastString (unpackFS)
import GHC
import GhcPlugins (Outputable, ppr, showSDocUnsafe)

getFixBinds :: LHsExpr GhcPs -> LHsBinds GhcPs
getFixBinds parsed =
  -- We know this deconstruction is safe, because we made it ourselves!
  -- but it never hurts to check
  assert (check parsed) $
    let ExprWithTySig _ par _ = unLoc parsed
        HsPar _ let' = unLoc par
        HsLet _ bs _ = unLoc let'
        HsValBinds _ vbs = unLoc bs
        ValBinds _ lbs _ = vbs
     in -- but it never hurts to check.
        lbs
  where
    -- The below pattern is the only one we expect and accept.
    check (L _ (ExprWithTySig _ (L _ (HsPar _ (L _ (HsLet _ (L _ (HsValBinds _ ValBinds {})) _)))) _)) = True
    check _ = False

applyFixes :: ParsedModule -> LHsBinds GhcPs -> (ParsedModule, [RFix])
applyFixes pm@ParsedModule {pm_parsed_source = (L lm hm@HsModule {..})} nb =
  (pm {pm_parsed_source = L lm (hm {hsmodDecls = decls'})}, swaps)
  where
    decls' = map swapDecl hsmodDecls
    swaps = mapMaybe swap hsmodDecls
    swap :: LHsDecl GhcPs -> Maybe RFix
    swap d@(L dl (ValD x FunBind {..})) =
      case nbMap Map.!? unLoc fun_id of
        Just b
          | n <- L dl (ValD x $ unLoc b),
            neq <- (/=) `on` (showSDocUnsafe . ppr),
            n `neq` d ->
            Just (d, n)
        _ -> Nothing
    swap _ = Nothing
    swapDecl d = maybe d snd (swap d)
    bToFunId :: LHsBind GhcPs -> Maybe (IdP GhcPs, LHsBind GhcPs)
    bToFunId b@(L _ FunBind {..}) = Just (unLoc fun_id, b)
    bToFunId _ = Nothing
    nbMap :: Map RdrName (LHsBind GhcPs)
    nbMap = Map.fromList $ mapMaybe bToFunId $ bagToList nb

-- | Colorize a pretty printed fix
colorizeDiff :: String -> String
colorizeDiff = unlines . map color . lines
  where
    color line@('+' : '+' : '+' : _) = line
    color line@('-' : '-' : '-' : _) = line
    color line@('-' : _) = red ++ line ++ nocolor
    color line@('+' : _) = green ++ line ++ nocolor
    color line@('!' : _) = orange ++ line ++ nocolor
    color line@('@' : '@' : _) = cyan ++ line ++ nocolor
    color l = l
    red = "\x1b[31m"
    green = "\x1b[32m"
    cyan = "\x1b[36m"

    orange = "\x1b[93m"
    nocolor = "\x1b[0m"

ppFix :: LHsExpr GhcPs -> EFix -> String
ppFix expr fixes = curry ppDiff expr $ replaceExpr fixes expr

-- | Pretty print a fix by adding git like '+' and '-' to each line.
ppDiff :: Outputable e => (Located e, Located e) -> String
ppDiff (L o1 d, L o2 d') =
  unlines
    ( unwords ["diff", "--git", f_a, f_b] :
      unwords ["---", f_a] :
      unwords ["+++", f_b] :
      range o1 o2 :
      toOut diffs
    )
  where
    f_a = "a/" ++ toLoc o1
    f_b = "b/" ++ toLoc o2
    diffs = zip (lines $ showUnsafe d) (lines $ showUnsafe d')
    toOut :: [(String, String)] -> [String]
    toOut [] = []
    toOut ((l, l') : ls) | l == l' = (' ' : l) : toOut ls
    toOut ((l, l') : ls) = ('-' : l) : ('+' : l') : toOut ls
    toLoc (RealSrcSpan rs) = unpackFS $ srcSpanFile rs
    toLoc (UnhelpfulSpan s) = unpackFS s
    header = case lines $ showUnsafe d of
      f : _ -> ' ' : if length f > 33 then take 30 f ++ "..." else f
      _ -> ""
    range (RealSrcSpan rs1) (RealSrcSpan rs2) =
      "@@ " ++ '-' : show s1 ++ ',' : d'' ++ ' ' : '+' : show s2 ++ ',' : d'' ++ " @@" ++ header
      where
        d'' = show $ length diffs
        s1 = srcSpanStartLine rs1
        s2 = srcSpanStartLine rs2
    -- Just a default
    range _ _ = "@@ -" ++ d'' ++ " +" ++ d'' ++ " @@" ++ header
      where
        d'' = show $ length diffs

fixesToDiffs :: ProblemDescription -> Set EFix -> [String]
fixesToDiffs desc@ProbDesc {probModule = Just modul} fixes =
  map (concatMap ppDiff . snd . applyFixes modul . getFixBinds) fixProgs
  where
    ProbDesc {..} = desc
    EProb {..} = progProblem
    -- We apply the same fixes to all the progs,
    -- so the first one should be the entire thing.
    (_, e_ty, e_prog') : _ = e_prog
    fixProgs = map (`replaceExpr` progAtTy e_prog' e_ty) $ Set.toList fixes
fixesToDiffs _ _ = error "Cannot print diff if module not available!"