-- These codes are experimentation
-- This is runnable standalone.

{-# LANGUAGE Arrows             #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}


import           BasicUnit
import           Bit
import           Control.Concurrent
import           Data.IORef
import           Debug.Trace
import           FRP.Yampa
import           Natural


main = testForRsff


-- RS flip-flop
-- TODO 最初の出力が(I, I)になってしまう（最初だけなので、大量に流すときは些細なことになると思うが）
-- TODO 前がresetの時にsetにすると(I, O)となってほしいが(I, I)となる。ハザードなのかもしれない
-- おそらく同じ信号をある程度（2連続ぐらい）流し続ける必要がありそう
rsff :: SF (Bit, Bit) (Bit, Bit)
rsff = proc (s, r) -> do
  s_    <- invGate -< s
  r_    <- invGate -< r
  rec
    preQ  <- dHold X -< Event q      -- init q  is X, and holds calculated value q
    preQ_ <- dHold X -< Event q_     -- init q_ is X, and holds calculated value q_
    q     <- nandGate -< (s_, preQ_)
    q_    <- nandGate -< (r_, preQ)
  returnA -< (q, q_)

-- RS flip-flop
-- TODO 最初の出力が(I, I)になってしまう（最初だけなので、大量に流すときは些細なことになると思うが）
-- TODO 前がresetの時にsetにすると(I, O)となってほしいが(I, I)となる。ハザードなのかもしれない
-- おそらく同じ信号をある程度（2連続ぐらい）流し続ける必要がありそう
rsff2 :: SF (Bit, Bit) (Bit, Bit)
rsff2 = proc (s, r) -> do
  s_    <- invGate -< s
  r_    <- invGate -< r
  rec
    preQ  <- dHold O -< Event q      -- init q  is X, and holds calculated value q
    preQ_ <- dHold O -< Event q_     -- init q_ is X, and holds calculated value q_
    q     <- nandGate -< (s_, preQ_)
    q_    <- nandGate -< (r_, preQ)
  returnA -< (q, q_)

-- RS flip-flop
-- TODO 最初の出力が(I, I)になってしまう（最初だけなので、大量に流すときは些細なことになると思うが）
-- TODO 前がresetの時にsetにすると(I, O)となってほしいが(I, I)となる。ハザードなのかもしれない
-- おそらく同じ信号をある程度（2連続ぐらい）流し続ける必要がありそう
norRsff :: SF (Bit, Bit) (Bit, Bit)
norRsff = proc (s, r) -> do
  rec
    preQ  <- dHold O -< Event q      -- init q  is X, and holds calculated value q
    preQ_ <- dHold O -< Event q_     -- init q_ is X, and holds calculated value q_
    q     <- norGate -< (r, preQ_)
    q_    <- norGate -< (s, preQ)
  returnA -< (q, q_)






-- -- RS Flip-Flop -- NOR Base
-- rsff :: SF (Bit, Bit) (Bit, Bit, (Bit, Bit))
-- rsff = proc (s, r) -> do
--   rec
--     preQ  <- dHold O -< Event q
--     preQ_ <- dHold I -< Event q_
--     q  <- norGate -< (s, preQ_)
--     q_ <- norGate -< (r, preQ)
--   returnA -< (q, q_, (preQ, preQ_))


-- Test for RS flip-flop
testForRsff = do
  let reset = (O, I)
      set   = (I, O)
      hold  = (O, O)
      tabu  = (I, I)
  -- let set   = (O, I)
  --     reset = (I, O)
  --     hold  = (O, O)
  --     tabu  = (I, I)

  print $ embed
    (norRsff) -- 使いたいSF
    -- (reset, [(0.1, Just e) | e <- [reset, hold, set, set]])--, set, hold, set, reset, reset, hold, hold, hold] ])
    (hold, [(0.1, Just e) | e <- [hold, set, set, reset, reset, reset, reset, hold, hold, set, set, hold, hold]])--, set, hold, set, reset, reset, hold, hold, hold] ])

orTest = do
  print $ I #| undefined -- I
  print $ X #| I         -- I
