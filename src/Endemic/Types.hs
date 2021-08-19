{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module      : Endemic.Types
-- Description : Holds the types used throughout the Endemic Library
-- License     : MIT
-- Stability   : experimental
--
-- This module holds common / shared types throughout the Endemic library.
-- Prefixes / Naming Convention:
-- - "R": Unparsed Elements, given by properties e.g. from console input or in
--        the first steps of test-evaluation
-- - "E": Expressions, as provided from GHC.
-- - "Ghc": Glasgow Haskell Compiler
-- - "LHs": Located Haskell, e.g. Located-Haskell-Binding. Located does not
--          necessarily mean "local", it refers to a piece of Haskell code at a
--          certain known position in a program, in contrag e.g. to the
--          synthesized fixed which are un-located at first.
-- - "Ps": Parsed, as a pass in GHC before compilation
-- - "Ty": Type
-- - "Wc": WildCard, for our purposes the "holes"
module Endemic.Types where

import Constraint (Cts)
import Control.DeepSeq (NFData (..))
import Data.Aeson
import Data.Default
import Data.Function (on)
import Data.Graph (Tree)
import Data.Map (Map, differenceWith)
import GHC
import GHC.Generics
import Outputable (Outputable (ppr), showSDocUnsafe, text, (<+>))
import qualified Outputable as O
import Trace.Hpc.Mix (BoxLabel)

-- |
-- Properties as in QuickCheck Properties.  Properties are strings, for now. We
-- parse them into LHsExpr GhcPs (=Expressions) later.
type RProp = String

type RContext = [String]

type RType = String

type RExpr = String

type RFix = (LHsDecl GhcPs, LHsDecl GhcPs)

-- A collection of things needed for a problem
data RProblem = RProb
  { r_props :: [RProp],
    r_ctxt :: RContext,
    r_target :: String,
    r_ty :: RType,
    r_prog :: RExpr
  }

type EProp = LHsBind GhcPs

type EContext = LHsLocalBinds GhcPs

type EType = LHsSigWcType GhcPs

type EExpr = LHsExpr GhcPs

-- | A Result from running a check.
-- +  Right True means all the checks were satisfied,
-- +  Right False is full failure (e.g., timeout or errors in the test-framework), and
-- + Left [Bool] expresses a list of the results of each run test,
--    where True is passing and False is failing.
type TestSuiteResult = Either [Bool] Bool

isFixed :: TestSuiteResult -> Bool
isFixed (Right x) = x
isFixed (Left xs) = and xs

type Trace = (LHsExpr GhcPs, Tree (SrcSpan, [(BoxLabel, Integer)]))

type TraceRes = [Trace]

-- | A fix is a list of replacements and their locations
type EFix = Map SrcSpan (HsExpr GhcPs)

-- From these we get equality on EFixes
instance Eq (HsExpr GhcPs) where
  (==) = (==) `on` showSDocUnsafe . ppr

instance Ord (HsExpr GhcPs) where
  compare = compare `on` showSDocUnsafe . ppr

instance NFData (HsExpr GhcPs) where
  rnf expr = seq (showSDocUnsafe (ppr expr)) ()

-- | An EProg contains a replacement for every target being fixed.
type EProg = [(RdrName, EType, EExpr)]

type EProgFix = [EExpr]

data EProblem
  = EProb
      { e_props :: [EProp],
        e_prop_sigs :: [LSig GhcPs],
        e_ctxt :: EContext,
        e_prog :: EProg,
        e_module :: Maybe ModuleName
      }
  | ExProb {ex_targets :: [Name]}

instance Outputable EProblem where
  ppr EProb {..} =
    text "EProblem {"
      <+> (text "props:" <+> ppr e_props)
      <+> (text "ctxt:" <+> ppr e_ctxt)
      <+> (text "prog: " <+> ppr e_prog)
      <+> text "}"
  ppr ExProb {..} =
    text "ExProblem {"
      <+> (text "target: " <+> ppr ex_targets)
      <+> text "}"

-- | ExprFitCands are used by the plugin to check whether an expression could fit
-- a given hole. Since they are not supported within the HoleFit framework, we
-- do it manually in the plugin.
data ExprFitCand = EFC
  { efc_cand :: LHsExpr GhcTc,
    efc_wc :: Cts,
    efc_ids :: [Id],
    efc_ty :: Maybe Type
  }

instance Outputable ExprFitCand where
  ppr EFC {..} = text "EFC {" O.<> ppr efc_cand O.<> text "}"

data LogLevel
  = DUMP
  | DEBUG
  | TRACE
  | AUDIT
  | TIMINGS
  | VERBOSE
  | INFO
  | GHCERR
  | WARN
  | ERROR
  | FATAL
  deriving (Eq, Ord, Read, Show, Generic, FromJSON, ToJSON)

instance Default LogLevel where
  def = WARN
