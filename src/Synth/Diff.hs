{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleContexts #-}
module Synth.Diff where

import GHC
import Bag
import Synth.Types
import Synth.Eval
import Synth.Repair (runJustParseExpr)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Maybe

getFixBinds :: CompileConfig -> String -> IO (LHsBinds GhcPs)
getFixBinds cc str =
   do parsed <- runJustParseExpr cc str
      -- We know this deconstruction is safe, because we made it ourselves!
      let ExprWithTySig _ par _ = unLoc parsed
          HsPar _ let' = unLoc par
          HsLet _ bs _ = unLoc let'
          HsValBinds _ vbs = unLoc bs
          ValBinds _ lbs sigs = vbs
      return lbs

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

prettyFix :: Bool -> RFix -> String
prettyFix color ((L orig d), (L _ d')) =
    showUnsafe orig ++ "\n"
    ++ unlines (toOut $ zip (lines $ showUnsafe d) (lines $ showUnsafe d'))
  where toL sym = map (sym:) . lines . showUnsafe
        toOut :: [(String, String)] -> [String]
        toOut [] = []
        toOut ((l,l'):ls) | l == l' = l:(toOut ls)
        toOut ((l,l'):ls) =
            (red   ++ ('-':l)):(green ++ ('+':l') ++ nocolor):(toOut ls)
        red = if color then "\x1b[31m" else ""
        green = if color then "\x1b[32m" else ""
        nocolor = if color then "\x1b[0m" else ""