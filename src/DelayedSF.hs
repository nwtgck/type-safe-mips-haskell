{-# LANGUAGE Arrows #-}

module DelayedSF(
  delayedSF
) where

import FRP.Yampa

-- Return Time delayed SF
delayedSF :: Eq b => Time -> b -> SF a b -> SF a b
delayedSF time init sf = proc sfIn -> do
  rec
    sfOut   <- sf -< sfIn
    pre     <- dHold init -< Event out
    delayed <- delay time init -< sfOut
    let out = if delayed == init then pre else delayed
  returnA -< out
