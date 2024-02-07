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

-- TODO: This doesn't recurse into (L _ (HsWrap _ _ v)), because there's no located expressions in v!

-- | Get this expression and all subexpressions
flattenExpr :: (id ~ GhcPs, Data (HsExpr id)) => LHsExpr id -> [LHsExpr id]
flattenExpr = universeOf uniplate


-- | Replace all expressions in a given expression with those
-- found in the given map.
replaceExpr :: (id ~ GhcPs, Data (HsExpr id))
            => Map SrcSpan (HsExpr id) -> LHsExpr id -> LHsExpr id
replaceExpr repls =
  transformOf uniplate $ \case
    L loc _ | l <- locA loc,
              l `member` repls -> L loc (repls ! l)
    e -> e

-- | Replace all expressions in a given expression with those
-- found in the given map.
wrapExpr :: (id ~ GhcPs, Data (HsExpr id))
         => SrcSpan -> (HsExpr id -> HsExpr id) -> LHsExpr id -> LHsExpr id
wrapExpr repl_loc trans =
  transformOf uniplate $ \case
    L loc x | l <- locA loc,
              l == repl_loc -> L loc (trans x)
    e -> e

-- | All possible replacement of one variable with a hole, i.e. we are making
-- the expression "holey". Which is pronounced holy.
-- Could also be named `perforate`, `stigmatize` or
-- `spindle`. See https://twitter.com/tritlo/status/1367202546415206400
sanctifyExpr ::
  (id ~ GhcPs, Data (HsExpr id), HasOccName (IdP id)) =>
  XUnboundVar id ->
  LHsExpr id ->
  [(SrcSpan, LHsExpr id)]
sanctifyExpr ext = map repl . contextsOf uniplate
  where
    repl ctxt = (loc, peek (reLocA (L loc hole)) ctxt)
      where
        (L loc expr) = reLoc $ pos ctxt
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
        locToStr (UnhelpfulSpan x) = show x
        locToStr s@(RealSrcSpan r _) =
          intercalate "_" $
            map show $
              [srcSpanStartLine r, srcSpanStartCol r]
                ++ ([srcSpanEndLine r | not (isOneLineSpan s)])
                ++ [srcSpanEndCol r]

-- | Fill the first hole in the given holed-expression.
fillHole :: (id ~ GhcPs, Data (HsExpr id)) => HsExpr id -> LHsExpr id -> Maybe (SrcSpan, LHsExpr id)
fillHole fit = fillFirst . contextsOf uniplate
  where
    fillFirst (ctxt : ctxts) =
      case pos ctxt of
        L loc (HsUnboundVar _ _) -> Just (locA loc, peek (L loc fit) ctxt)
        _ -> fillFirst ctxts
    fillFirst [] = Nothing
