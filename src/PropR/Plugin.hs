{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : PropR.Plugin
-- Description : Moves some of the PropR into the GHC compilation
-- License     : MIT
-- Stability   : experimental
-- This module introduces the PropR-Plugin to fasten up checking for the correct
-- expressions in GHC. It also introduces heuristic caching.
module PropR.Plugin where

import Bag (unionBags)
import Constraint
import Control.Arrow (first, second)
import Control.Monad (filterM)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map as Map
import Data.Maybe (isJust, isNothing)
import GhcPlugins
import PropR.Configuration (CompileConfig (CompConf))
import PropR.Types (ExprFitCand (..), LogLevel (..))
import PropR.Util (logOut, logStr, traceOut)
import TcHoleErrors
import TcRnMonad
import TyCoFVs (tyCoFVsOfTypes)

type HoleHash = (RealSrcSpan, String)

-- We're running the same checks over and over again, so we make the (possibly
-- unsafe) assumption  that if we're looking for fits for the same type at the
-- same location, then we can just skip the checks and return the fits directly.
-- This should be OK, since our fits do not change the types, and the repairs do
-- not introduce additional bindings into scope.
type HoleFitCache = Map.Map (Int, HoleHash) (TypedHole, [HoleFit])

type HoleFitList = [(TypedHole, [HoleFit])]

type HoleFitState = (HoleFitCache, HoleFitList)

resetHoleFitCache :: HoleFitState -> HoleFitState
resetHoleFitCache = first (const Map.empty)

resetHoleFitList :: HoleFitState -> HoleFitState
resetHoleFitList = second (const [])

initialHoleFitState :: HoleFitState
initialHoleFitState = (Map.empty, [])

-- | Provides a heuristic Hash for the Typed holes, used for lookup in Caching.
holeHash :: DynFlags -> Maybe [Type] -> TypedHole -> Maybe HoleHash
holeHash df defaults TyH {tyHCt = Just ct} =
  Just (ctLocSpan $ ctLoc ct, showSDocOneLine df $ ppr (defaults, ctPred ct))
holeHash _ _ _ = Nothing

synthPlug :: CompileConfig -> Bool -> [ExprFitCand] -> IORef HoleFitState -> Plugin
synthPlug CompConf {..} useCache local_exprs plugRef =
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
                        cache <- liftIO $ fst <$> readIORef plugRef
                        dflags <- getDynFlags
                        num_calls <- readTcRef iref
                        -- We change the defaults, so they must be included
                        -- in the hash.
                        defaults <- tcg_default <$> getGblEnv
                        case holeHash dflags defaults h >>= (cache Map.!?) . (num_calls,) of
                          Just r | useCache -> liftIO $ do
                            logStr DEBUG "CACHE HIT"
                            logOut DEBUG $ holeHash dflags defaults h
                            logOut DEBUG (num_calls, h)
                            logOut DEBUG r
                            return []
                          _ -> return c,
                    fitPlugin = \h f -> do
                      cache <- liftIO $ fst <$> readIORef plugRef
                      dflags <- getDynFlags
                      defaults <- tcg_default <$> getGblEnv
                      num_calls <- readTcRef iref
                      writeTcRef iref (num_calls + 1)
                      gbl_env <- getGlobalRdrEnv
                      let lcl_env =
                            case tyHCt h of
                              Just ct -> tcl_rdr $ ctLocEnv (ctEvLoc (ctEvidence ct))
                              _ -> emptyLocalRdrEnv
                          -- A name is in scope if it's in the local or global environment
                          inScope e_id
                            | e_name <- getName e_id,
                              e_occ <- occName e_id =
                              isWiredInName e_name
                                || if isLocalId e_id
                                  then -- A local variable is in scope if it's in the environment
                                    inLocalRdrEnvScope e_name lcl_env
                                  else -- A global variable is in scope if it's not shadowed by a local:
                                  -- or if it's wired in.

                                    not (null (lookupGlobalRdrEnv gbl_env e_occ))
                                      && isNothing (lookupLocalRdrOcc lcl_env e_occ)
                      case holeHash dflags defaults h >>= (cache Map.!?) . (num_calls,) of
                        Just cached | useCache -> liftIO $ do
                          modifyIORef' plugRef (fmap (cached :))
                          return []
                        _ -> do
                          -- Bump the number of times this plugin has been called, used
                          -- to make sure we only check expression fits once.
                          exprs <- case tyHCt h of
                            -- We only do fits for non-refinement hole fits, i.e. the first time the plugin is called.
                            Just ct | num_calls == 0 -> do
                              -- Since ExprFitCands are not supported in GHC yet  we manually implement them here by
                              -- checking that all the variables in the expression are in scope and that the type matches.
                              let in_scope_exprs = filter (all inScope . efc_ids) local_exprs
                                  hole_ty = ctPred ct
                                  -- An expression candidate fits if its type matches and there are no unsolved
                                  -- wanted constraints afterwards.
                                  checkExprCand :: ExprFitCand -> TcM Bool
                                  checkExprCand EFC {efc_ty = Nothing} = return False
                                  checkExprCand EFC {efc_ty = Just e_ty, efc_wc = rcts} =
                                    fst <$> withoutUnification fvs (tcCheckHoleFit h {tyHRelevantCts = cts} hole_ty e_ty)
                                    where
                                      fvs = tyCoFVsOfTypes [hole_ty, e_ty]
                                      cts = tyHRelevantCts h `unionBags` rcts
                              map efc_cand <$> filterM checkExprCand in_scope_exprs
                            _ -> return []

                          let fits = map (RawHoleFit . ppr) exprs ++ filter isOk f
                              isOk HoleFit {..}
                                | GreHFCand elt <- hfCand,
                                  occ <- greOccName elt,
                                  [_] <- lookupGlobalRdrEnv gbl_env occ,
                                  Nothing <- lookupLocalRdrOcc lcl_env occ =
                                  True
                              isOk HoleFit {..}
                                | GreHFCand elt <- hfCand,
                                  occ <- greOccName elt,
                                  [GRE {..}] <- lookupGlobalRdrEnv gbl_env occ,
                                  Just n <- lookupLocalRdrOcc lcl_env occ =
                                  getLoc n == getLoc gre_name
                              isOk HoleFit {..} | inScope hfId = True
                              isOk RawHoleFit {} = True
                              isOk x = False
                          liftIO $ do
                            let packed = (h, fits)
                            modifyIORef' plugRef (fmap (packed :))
                            case holeHash dflags defaults h of
                              Just hash | useCache -> do
                                logStr DEBUG "CACHING"
                                logOut DEBUG (num_calls, hash)
                                logOut DEBUG packed
                                modifyIORef' plugRef (first (Map.insert (num_calls, hash) packed))
                              _ -> return ()
                          return fits
                  }
            }
    }
