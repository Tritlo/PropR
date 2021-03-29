{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleContexts #-}
module Synth.Diff where

import GHC
import Bag
import Synth.Types
import Synth.Eval

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe
import Control.Exception


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
        -- but it never hurts to check.
        in  lbs
  where check (L _ (ExprWithTySig _
               (L _ (HsPar _
                (L _ (HsLet _
                 (L _ (HsValBinds _
                        (ValBinds _ _ _))) _)))) _)) = True
        check _ = False

applyFixes :: ParsedModule -> LHsBinds GhcPs -> (ParsedModule, [RFix])
applyFixes pm@ParsedModule{pm_parsed_source=(L lm (hm@HsModule{..}))} nbs =
    (pm {pm_parsed_source=(L lm (hm {hsmodDecls = decls'}) )}, swaps)
    where decls' = map swapDecl hsmodDecls
          swaps = mapMaybe swap hsmodDecls
          swap :: LHsDecl GhcPs -> Maybe RFix
          swap d@(L dl (ValD x FunBind{..}))
            = case nbMap Map.!? (unLoc fun_id) of
                Just b -> Just (d, L dl (ValD x $ unLoc b))
                _ -> Nothing
          swap _ = Nothing
          swapDecl d = maybe d snd (swap d)
          bToFunId :: LHsBind GhcPs -> Maybe (IdP GhcPs, LHsBind GhcPs)
          bToFunId b@(L _ FunBind{..}) = Just (unLoc fun_id, b)
          bToFunId _ = Nothing
          nbMap :: Map RdrName (LHsBind GhcPs)
          nbMap = Map.fromList $ mapMaybe bToFunId $ bagToList nbs


-- Colorize a pretty printed fix
colorizeDiff:: String -> String
colorizeDiff= unlines . map color . lines
  where color line@('-':_) = red ++ line ++ nocolor
        color line@('+':_) = green ++ line ++ nocolor
        color l = l
        red     = "\x1b[31m"
        green   = "\x1b[32m"
        nocolor = "\x1b[0m"

-- Pretty print a fix by adding git like '+' and '-' to each line.
ppDiff:: RFix -> String
ppDiff ((L orig d), (L _ d')) =
    showUnsafe orig ++ "\n"
    ++ unlines (toOut $ zip (lines $ showUnsafe d) (lines $ showUnsafe d'))
  where toL sym = map (sym:) . lines . showUnsafe
        toOut :: [(String, String)] -> [String]
        toOut [] = []
        toOut ((l,l'):ls) | l == l' = l:(toOut ls)
        toOut ((l,l'):ls) = (('-':l)):(('+':l')):(toOut ls)
