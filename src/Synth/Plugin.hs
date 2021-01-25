module Synth.Plugin where

import GhcPlugins hiding (TcPlugin)
import TcHoleErrors
import Data.IORef

synthPlug :: IORef [(TypedHole, [HoleFit])] -> Plugin
synthPlug plugRef =
  defaultPlugin {
    holeFitPlugin = \_ -> Just $
      fromPureHFPlugin $
        HoleFitPlugin {
              candPlugin = (\_ c -> do return c)
            , fitPlugin = (\h f -> do liftIO $ modifyIORef plugRef ((h,f):)
                                      return f)}}
