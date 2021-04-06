{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Synth.Traversals where

import Control.Comonad.Store.Class
import Control.Lens
import Data.Data
import Data.Data.Lens
import Data.List (intercalate)
import Data.Map (Map, member, (!))
import Data.Maybe (listToMaybe, mapMaybe)
import GHC
import GhcPlugins
  ( HasOccName (occName),
    OccName (occNameFS, occNameSpace),
    concatFS,
    fsLit,
    isOneLineSpan,
    mkOccNameFS,
    mkVarOcc,
    unpackFS,
  )

-- We use lenses to avoid having to manually write the traversals.
instance Data (HsExpr id) => Plated (LHsExpr id) where
  plate = uniplate

-- Get this expression and all subexpressions
flattenExpr :: Data (HsExpr id) => LHsExpr id -> [LHsExpr id]
flattenExpr = universe

-- Replace all expressions in a given expression with those
-- found in the given map.
replaceExpr :: Map SrcSpan (HsExpr GhcPs) -> LHsExpr GhcPs -> LHsExpr GhcPs
replaceExpr repls =
  transform $ \case
    L loc _ | loc `member` repls -> L loc (repls ! loc)
    e -> e

-- All possible replacement of one variable with a hole, i.e. we are making
-- the expression "holey". Could also be named `perforate`, `stigmatize` or
-- `spindle`. See https://twitter.com/tritlo/status/1367202546415206400
sanctifyExpr :: LHsExpr GhcPs -> [(SrcSpan, LHsExpr GhcPs)]
sanctifyExpr = map repl . contexts
  where
    repl ctxt = (loc, peek (L loc hole) ctxt)
      where
        (L loc expr) = pos ctxt
        hole = HsUnboundVar noExtField $ TrueExprHole name
        name = case expr of
          HsVar x (L _ v) ->
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

-- Fill the first hole in the expression.
fillHole :: HsExpr GhcPs -> LHsExpr GhcPs -> Maybe (SrcSpan, LHsExpr GhcPs)
fillHole fit = fillFirst . contexts
  where
    fillFirst (ctxt : ctxts) =
      case pos ctxt of
        L loc (HsUnboundVar _ _) -> Just (loc, peek (L loc fit) ctxt)
        _ -> fillFirst ctxts
    fillFirst [] = Nothing