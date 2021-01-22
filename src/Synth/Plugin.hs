module Synth.Plugin where

import GhcPlugins hiding (TcPlugin)
import TcHoleErrors

synthPlug :: Plugin
synthPlug = defaultPlugin {
            holeFitPlugin = \_ -> Just $
                fromPureHFPlugin (
                  HoleFitPlugin {
                      candPlugin = (\_ c -> liftIO (putStrLn "here!") >> return c)
                    , fitPlugin = (\h f -> return f) })
           }
