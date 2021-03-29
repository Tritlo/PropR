{-# LANGUAGE RecordWildCards #-}
module Synth.Plugin where

import GhcPlugins hiding (TcPlugin)
import TcHoleErrors
import Data.IORef
import Constraint
import GHC
import TcRnMonad
import Control.Monad (filterM)
import TyCoFVs (tyCoFVsOfTypes)

import TcRnTypes
import RnExpr
import TcExpr
import Data.Maybe (isJust)
import Synth.Types
import Bag

synthPlug :: [ExprFitCand] -> IORef [(TypedHole, [HoleFit])] -> Plugin
synthPlug local_exprs plugRef =
  defaultPlugin {
    holeFitPlugin = \_ -> Just $
        HoleFitPluginR {
        hfPluginInit = newTcRef (0 :: Int)
        , hfPluginStop = \_ -> return ()
        , hfPluginRun = \iref -> HoleFitPlugin {
              candPlugin = \_ c -> return c
            , fitPlugin = \h f -> do
                 -- Bump the number of times this plugin has been called, used
                 -- to make sure we only check expression fits once.
                 num_calls <- readTcRef iref
                 writeTcRef iref (num_calls + 1)
                 dflags <- getDynFlags
                 exprs <- case tyHCt h of
                            -- We only do fits for non-refinement hole fits, i.e. the first time the plugin is called.
                            Just ct | num_calls == 0->
                               -- Since ExprFitCands are not supported in GHC yet  we manually implement them here by
                               -- checking that all the variables in the expression are in scope and that the type matches.
                               do gbl_env <- getGlobalRdrEnv
                                  let lcl_env = tcl_rdr $ ctLocEnv (ctEvLoc (ctEvidence ct))
                                      -- A name is in scope if it's in the local or global environment
                                      inScope nm = nm `inLocalRdrEnvScope` lcl_env || isJust (lookupGRE_Name gbl_env nm)
                                      in_scope_exprs = filter (all (inScope . getName) . efc_ids) local_exprs
                                      hole_ty = ctPred ct
                                      -- An expression candidate fits if its type matches and there are no unsolved
                                      -- wanted constraints afterwards.
                                      checkExprCand :: ExprFitCand -> TcM Bool
                                      checkExprCand EFC{efc_ty=Nothing} = return False
                                      checkExprCand EFC{efc_ty=Just e_ty, efc_wc = rcts} = fst <$>
                                        withoutUnification fvs (tcCheckHoleFit h {tyHRelevantCts=cts} hole_ty e_ty)
                                        where fvs = tyCoFVsOfTypes [hole_ty, e_ty]
                                              cts = tyHRelevantCts h `unionBags`rcts
                                  map efc_cand <$> filterM checkExprCand in_scope_exprs
                            _ -> return []
                 let fits = map (RawHoleFit . ppr) exprs ++ f
                 liftIO $ modifyIORef plugRef ((h,fits):)
                 return fits}}}
