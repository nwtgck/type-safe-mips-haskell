-- Yampaを使って論理回路を作る
-- TODO Bitsに長さを含めて型付けする（ビット違いの配線ミスを防ぐため）(多少型付けした)

{-# LANGUAGE Arrows #-}

import           Control.Concurrent
import           Control.Monad
import           Data.Bits          (shift, (.|.))
import           Data.IORef
import           Data.Maybe
import           Debug.Trace
import           FRP.Yampa

data N0
data Succ n
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4

-- O, I, X mean 0, 1, x respectively
data Bit = O | I | X deriving Show

data Bits n = Bits [Bit] deriving Show -- Bits synonim can be changed, so I use the synonim

-- Bit NOT
inv O = I
inv I = O
inv X = X

-- -- Bit OR
-- I #| _ = I
-- _ #| I = I
-- O #| O = O
-- _ #| _ = X
--
-- -- Bit AND
-- O #& _ = O
-- I #& I = I
-- I #& _ = O
-- _ #& _ = X

-- Bit OR
(#|) :: Bit -> Bit -> Bit
O #| O = O
_ #| _ = I

-- Bit AND
(#&) :: Bit -> Bit -> Bit
I #& I = I
_ #& _ = O

-- Bit XOR
(#^) :: Bit -> Bit -> Bit
I #^ I = O
O #^ O = O
X #^ _ = X
_ #^ X = X
_ #^ _ = I

-- AND gate
andGate :: SF (Bit, Bit) Bit
andGate = proc (i1, i2) -> do
  returnA -< i1 #& i2

-- OR gate
orGate :: SF (Bit, Bit) Bit
orGate = proc (i1, i2) -> do
  returnA -< i1 #| i2

-- XOR gate
xorGate :: SF (Bit, Bit) Bit
xorGate = proc (i1, i2) -> do
  returnA -< i1 #^ i2

-- NOT gate
invGate :: SF Bit Bit
invGate = proc b -> do
  returnA -< inv b

-- NAND gate
nandGate :: SF (Bit, Bit) Bit
nandGate = proc (a, b) -> do
  ando <- andGate -< (a, b)
  invGate -< ando

-- Tracer for debugging
tracer :: Show a => SF a a
tracer = proc a -> do
  returnA -< trace ("trace{" ++ show a ++ "}") a

-- Half Adder
halfAdder :: SF (Bit, Bit) (Bit, Bit)
halfAdder = proc (a, b) -> do
  sum   <- xorGate -< (a, b)
  carry <- andGate -< (a, b)
  returnA -< (carry, sum)

-- Full Adder
fullAdder :: SF (Bit, Bit, Bit) (Bit, Bit)
fullAdder = proc (a, b, cin) -> do
  (c0, s0) <- halfAdder -< (a, b)
  (c1, s1) <- halfAdder -< (s0, cin)
  returnA -< (c0 #| c1, s1)

-- 4 bits Adder
add4Bits :: SF (Bits N4, Bits N4) (Bits N5)
add4Bits = proc (Bits [a3,a2,a1,a0], Bits [b3,b2,b1,b0]) -> do
  (c0, s0) <- halfAdder -< (a0, b0)
  (c1, s1) <- fullAdder -< (a1, b1, c0)
  (c2, s2) <- fullAdder -< (a2, b2, c1)
  (c3, s3) <- fullAdder -< (a3, b3, c2)
  returnA -< Bits [c3,s3,s2,s1,s0]

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

-- Converter for Bits to Int number
bitsToIntMay :: Bits a -> Maybe Int
bitsToIntMay (Bits bs) = foldM (\s b -> case b of
    O -> Just $ s `shift` 1 .|. 0
    I -> Just $ s `shift` 1 .|. 1
    X  -> Nothing
  ) 0 bs

-- -- Converter for [Int] to Bits
-- toBits :: [Int] -> Bits a
-- toBits = Bits $ fmap (\e -> if e == 1 then I else O)


-- TODO use range(but not implemented)
bitsDrop n (Bits bs) = Bits $ drop n bs

-- Test for 4 bits adder
testForAdd4bits = do
  prevOut <- newIORef (undefined :: Bits N5)
  let zero = (Bits [O,O,O,O] :: Bits N4)
      one  = (Bits [O,O,O,I] :: Bits N4)
  reactimate (return (zero, zero))
               (\_ -> do
                 threadDelay 100000
                 out <- readIORef prevOut
                 return (0.1, (Just $ (bitsDrop 1 out, one) )))
               (\_ out -> print (out, bitsToIntMay out) >> writeIORef prevOut out >> return False)
               (add4Bits)

-- Test for RS flip-flop
testForRsff = do
  let reset = (O, I)
      set   = (I, O)
      hold  = (O, O)
      tabu  = (I, I)

  print $ embed
    (rsff) -- 使いたいSF
    (reset, [(0.1, Just e) | e <- [reset, set, set, hold, set, reset, reset, hold, hold, hold] ])

orTest = do
  print $ I #| undefined -- I
  print $ X #| I         -- I

main :: IO ()
main = testForRsff
