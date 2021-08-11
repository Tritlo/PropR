{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : Endemic.Traversal
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
module Endemic.Traversals where

import Control.Comonad.Store.Class (ComonadStore (peek, pos))
import Control.Lens (Plated (..), contexts, contextsOf, transform, transformOf, universe, universeOf)
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Data.List (intercalate)
import Data.Map (Map, member, (!))
import GHC
import GhcPlugins

-- TODO: This doesn't recurse into (L _ (HsWrap _ _ v)), because there's no located expressions in v!

-- | Get this expression and all subexpressions
flattenExpr :: Data (HsExpr id) => LHsExpr id -> [LHsExpr id]
flattenExpr = universeOf uniplate

-- | Replace all expressions in a given expression with those
-- found in the given map.
replaceExpr :: Map SrcSpan (HsExpr GhcPs) -> LHsExpr GhcPs -> LHsExpr GhcPs
replaceExpr repls =
  transformOf uniplate $ \case
    L loc _ | loc `member` repls -> L loc (repls ! loc)
    e -> e

-- | Replace all expressions in a given expression with those
-- found in the given map.
wrapExpr :: SrcSpan -> (HsExpr GhcPs -> HsExpr GhcPs) -> LHsExpr GhcPs -> LHsExpr GhcPs
wrapExpr repl_loc trans =
  transformOf uniplate $ \case
    L loc x | loc == repl_loc -> L loc (trans x)
    e -> e

-- | All possible replacement of one variable with a hole, i.e. we are making
-- the expression "holey". Which is pronounced holy.
-- Could also be named `perforate`, `stigmatize` or
-- `spindle`. See https://twitter.com/tritlo/status/1367202546415206400
sanctifyExpr :: LHsExpr GhcPs -> [(SrcSpan, LHsExpr GhcPs)]
sanctifyExpr = map repl . contextsOf uniplate
  where
    repl ctxt = (loc, peek (L loc hole) ctxt)
      where
        (L loc expr) = pos ctxt
        hole = HsUnboundVar noExtField $ TrueExprHole name
        name = case expr of
          HsVar _ (L _ v) ->
            let (ns, fs) = (occNameSpace (occName v), occNameFS (occName v))
             in mkOccNameFS ns (concatFS $ fsLit "_" : [fs, fsLit $ locToStr loc])
          _ -> mkVarOcc $ "_" ++ locToStr loc
        locToStr (UnhelpfulSpan x) = unpackFS x
        locToStr s@(RealSrcSpan r) =
          intercalate "_" $
            map show $
              [srcSpanStartLine r, srcSpanStartCol r]
                ++ ([srcSpanEndLine r | not (isOneLineSpan s)])
                ++ [srcSpanEndCol r]

-- | Fill the first hole in the given holed-expression.
fillHole :: HsExpr GhcPs -> LHsExpr GhcPs -> Maybe (SrcSpan, LHsExpr GhcPs)
fillHole fit = fillFirst . contextsOf uniplate
  where
    fillFirst (ctxt : ctxts) =
      case pos ctxt of
        L loc (HsUnboundVar _ _) -> Just (loc, peek (L loc fit) ctxt)
        _ -> fillFirst ctxts
    fillFirst [] = Nothing
