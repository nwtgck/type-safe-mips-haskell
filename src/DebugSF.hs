{-# LANGUAGE Arrows #-}

module DebugSF(
  tracer
) where

import           Debug.Trace
import           FRP.Yampa


-- Tracer for debugging
tracer :: Show a => SF a a
tracer = proc a -> do
  returnA -< trace ("trace{" ++ show a ++ "}") a
