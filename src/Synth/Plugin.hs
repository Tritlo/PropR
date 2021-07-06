{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Synth.Plugin
-- Description : Moves some of the Endemic into the GHC compilation
-- License     : MIT
-- Stability   : experimental
-- This module introduces the Synth-Plugin to fasten up checking for the correct
-- expressions in GHC. It also introduces heuristic caching.
module Synth.Plugin where

import Bag
import Constraint
import Control.Monad (filterM)
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (isJust)
import GHC
import GhcPlugins hiding (TcPlugin)
import RnExpr
import Synth.Types
import Synth.Util (LogLevel (AUDIT, DEBUG), logOut, logStr)
import System.IO.Unsafe (unsafePerformIO)
import TcExpr
import TcHoleErrors
import TcRnMonad
import TcRnTypes
import TyCoFVs (tyCoFVsOfTypes)

-- We're running the same checks over and over again, so we make the (possibly
-- unsafe) assumption  that if we're looking for fits for the same type at the
-- same location, then we can just skip the checks and return the fits directly.
-- This should be OK, since our fits do not change the types, and the repairs do
-- not introduce additional bindings into scope.
holeFitCache :: IORef (Map.Map (Int, HoleHash) (TypedHole, [HoleFit]))
{-# NOINLINE holeFitCache #-}
holeFitCache = unsafePerformIO $ newIORef Map.empty

type HoleHash = (RealSrcSpan, String)

-- | Provides a heuristic Hash for the Typed holes, used for lookup in Caching.
holeHash :: DynFlags -> TypedHole -> Maybe HoleHash
holeHash df TyH {tyHCt = Just ct} =
  Just (ctLocSpan $ ctLoc ct, showSDocOneLine df $ ppr $ ctPred ct)
holeHash _ _ = Nothing

synthPlug :: Bool -> [ExprFitCand] -> IORef [(TypedHole, [HoleFit])] -> Plugin
synthPlug useCache local_exprs plugRef =
  defaultPlugin
    { holeFitPlugin = \_ ->
        Just $
          HoleFitPluginR
            { hfPluginInit = newTcRef (0 :: Int),
              hfPluginStop = \_ -> return (),
              hfPluginRun = \iref ->
                HoleFitPlugin
                  { candPlugin =
                      \h c -> do
                        cache <- liftIO $ readIORef holeFitCache
                        dflags <- getDynFlags
                        num_calls <- readTcRef iref
                        case holeHash dflags h >>= (cache Map.!?) . (num_calls,) of
                          Just r | useCache -> liftIO $ do
                            logStr DEBUG "CACHE HIT"
                            logOut DEBUG $ holeHash dflags h
                            logOut DEBUG (num_calls, h)
                            logOut DEBUG r
                            return []
                          _ -> return c,
                    fitPlugin = \h f -> do
                      cache <- liftIO $ readIORef holeFitCache
                      dflags <- getDynFlags
                      num_calls <- readTcRef iref
                      writeTcRef iref (num_calls + 1)
                      case holeHash dflags h >>= (cache Map.!?) . (num_calls,) of
                        Just cached | useCache -> liftIO $ do
                          modifyIORef plugRef (cached :)
                          return []
                        _ -> do
                          -- Bump the number of times this plugin has been called, used
                          -- to make sure we only check expression fits once.
                          exprs <- case tyHCt h of
                            -- We only do fits for non-refinement hole fits, i.e. the first time the plugin is called.
                            Just ct | num_calls == 0 -> do
                              -- Since ExprFitCands are not supported in GHC yet  we manually implement them here by
                              -- checking that all the variables in the expression are in scope and that the type matches.
                              gbl_env <- getGlobalRdrEnv
                              let lcl_env = tcl_rdr $ ctLocEnv (ctEvLoc (ctEvidence ct))
                                  -- A name is in scope if it's in the local or global environment
                                  inScope nm = nm `inLocalRdrEnvScope` lcl_env || isJust (lookupGRE_Name gbl_env nm)
                                  in_scope_exprs = filter (all (inScope . getName) . efc_ids) local_exprs
                                  hole_ty = ctPred ct
                                  -- An expression candidate fits if its type matches and there are no unsolved
                                  -- wanted constraints afterwards.
                                  checkExprCand :: ExprFitCand -> TcM Bool
                                  checkExprCand EFC {efc_ty = Nothing} = return False
                                  checkExprCand EFC {efc_ty = Just e_ty, efc_wc = rcts} =
                                    fst
                                      <$> withoutUnification fvs (tcCheckHoleFit h {tyHRelevantCts = cts} hole_ty e_ty)
                                    where
                                      fvs = tyCoFVsOfTypes [hole_ty, e_ty]
                                      cts = tyHRelevantCts h `unionBags` rcts
                              map efc_cand <$> filterM checkExprCand in_scope_exprs
                            _ -> return []
                          let fits = map (RawHoleFit . ppr) exprs ++ f
                          liftIO $ do
                            let packed = (h, fits)
                            modifyIORef plugRef (packed :)
                            case holeHash dflags h of
                              Just hash | useCache -> do
                                let upd (Just prev) = Just (packed : prev)
                                    upd _ = Just [packed]
                                logStr DEBUG "CACHING"
                                logOut DEBUG (num_calls, hash)
                                logOut DEBUG packed
                                modifyIORef holeFitCache (Map.insert (num_calls, hash) packed)
                              _ -> return ()
                          return fits
                  }
            }
    }
