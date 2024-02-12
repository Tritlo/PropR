{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : PropR.Traversal
-- Description : Replaces holes in functions with other found expressions.
-- License     : MIT
-- Stability   : experimental
--
-- This module has the methods to
-- a) Retrieve all suited expressions for a hole
-- b) Put another expression in a fitting hole
--
-- It is called Traversal as all included methods traverse the expressions and
-- sub-expressions.
--
-- Both happens on a low level, not on the module/compilation level.
-- This is a pure module.
module PropR.Traversals where

import Control.Comonad.Store.Class (ComonadStore (peek, pos))
import Control.Lens (Plated (..), contexts, contextsOf, transform, transformOf, universe, universeOf)
import Data.Char (isAlphaNum, ord)
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Data.List (intercalate)
import Data.Map (Map, member, (!))
import GHC
import GHC.Plugins
import PropR.Types ()

import GHC.Tc.Errors.Hole.FitTypes (TypedHole (..))
import GHC.Tc.Types.Constraint (Hole(..))

-- TODO: This doesn't recurse into (L _ (HsWrap _ _ v)), because there's no located expressions in v!

-- | Get this expression and all subexpressions
flattenExpr :: (Data (HsExpr id), Data (XRec id (HsExpr id)))
            => LHsExpr id -> [LHsExpr id]
flattenExpr = universeOf uniplate



-- | Replace all expressions in a given expression with those
-- found in the given map.
replaceExpr :: (id ~ GhcPs, Data (HsExpr id))
            => Map (SrcAnn AnnListItem) (HsExpr id) -> LHsExpr id -> LHsExpr id
replaceExpr repls =
  transformOf uniplate $ \case
    L loc _ | loc `member` repls -> L loc (repls ! loc)
    e -> e

-- | Replace all expressions in a given expression with those
-- found in the given map.
wrapExpr :: (id ~ GhcPs, Data (HsExpr id))
         => SrcAnn AnnListItem -> (HsExpr id -> HsExpr id) -> LHsExpr id -> LHsExpr id
wrapExpr repl_loc trans =
  transformOf uniplate $ \case
    L loc x | (rbf loc) == (rbf repl_loc) -> L loc (trans x)
    e -> e
  where rbf = removeBufSpan . locA

-- | All possible replacement of one variable with a hole, i.e. we are making
-- the expression "holey". Which is pronounced holy.
-- Could also be named `perforate`, `stigmatize` or
-- `spindle`. See https://twitter.com/tritlo/status/1367202546415206400
sanctifyExpr ::
  (id ~ GhcPs, Data (HsExpr id), HasOccName (IdP id)) =>
  XUnboundVar id ->
  LHsExpr id ->
  [(SrcAnn AnnListItem, LHsExpr id)]
sanctifyExpr ext = map repl . contextsOf uniplate
  where
    repl ctxt = (loc, peek (L loc hole) ctxt)
      where
        (L loc expr) = pos ctxt
        hole = HsUnboundVar ext (mkRdrUnqual name)
        name = case expr of
          HsVar _ (L _ v) ->
            let (ns, fs) = (occNameSpace (occName v), fsLit (sanitize v))
             in mkOccNameFS ns (concatFS $ fsLit "_" : [fs, fsLit $ locToStr loc])
          _ -> mkVarOcc $ "_" ++ locToStr loc
        sanitize nm =
          if not (null alphanum)
            then alphanum
            else intercalate "_" $ map (show . ord) base
          where
            base = occNameString $ occName nm
            alphanum = filter isAlphaNum base
        locToStr = locToStr' . locA
        locToStr' (UnhelpfulSpan x) = show x
        locToStr' s@(RealSrcSpan r _) =
          intercalate "_" $
            map show $
              [srcSpanStartLine r, srcSpanStartCol r]
                ++ ([srcSpanEndLine r | not (isOneLineSpan s)])
                ++ [srcSpanEndCol r]

-- | Fill the first hole in the given holed-expression.
fillHole :: (id ~ GhcPs, Data (HsExpr id)) =>
        Maybe TypedHole -> -- The hole to be filled, if provided.
                           -- Otherwise we just fill the first hole.
        HsExpr id -> LHsExpr id -> Maybe (SrcAnn AnnListItem, LHsExpr id)
fillHole mb_th fit = fill . contextsOf uniplate
  where
    fill (ctxt : ctxts) =
      case pos ctxt of
        L loc (HsUnboundVar _ occ) | matchesHole occ mb_th ->
            Just (loc, peek (L loc fit) ctxt)
        _ -> fill ctxts
      where
        matchesHole _ Nothing = True
        matchesHole occ
            (Just TypedHole{th_hole=Just Hole{hole_occ = h1}}) = h1 == occ
        matchesHole occ _ = False

    fill [] | Nothing <- mb_th = Nothing
    fill [] = error "fillHole: hole not present in expression!"
