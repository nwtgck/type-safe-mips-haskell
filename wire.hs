-- Yampaを使って論理回路を作る
-- TODO Bitsに長さを含めて型付けする（ビット違いの配線ミスを防ぐため）(多少型付けした)

{-# LANGUAGE Arrows             #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}

import           Control.Concurrent
import           Control.Monad
import           Data.Bits          (shift, (.|.))
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           FRP.Yampa
import           FRP.Yampa.Event

data N0
data Succ n
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4

-- Singleton for N0 and Succ n
data SNat n :: * where
  SN0   :: SNat N0
  SSucc :: SNat n ->  SNat (Succ n)
deriving instance Show (SNat n)

n0 = SN0
n1 = SSucc n0
n2 = SSucc n1
n3 = SSucc n2
n4 = SSucc n3
n5 = SSucc n4
n6 = SSucc n5
n7 = SSucc n6

(#+) :: SNat n -> SNat m -> SNat (n + m)
SN0 #+ b   = b
(SSucc a) #+ b = SSucc (a #+ b)
infixl 6 #+

(#-) :: SNat n -> SNat m -> SNat (n - m)
SN0 #- b               = n0
a   #- SN0             = a
(SSucc a) #- (SSucc b) = a #-b

-- (SSucc a) #+ b = SSucc (a #+ b)
infixl 6 #-

-- Nat to SNat converter
class NatToSNat n where
  natToSNat :: n -> SNat n

instance NatToSNat N0 where
  natToSNat _ = n0

instance NatToSNat n => NatToSNat (Succ n) where
  natToSNat _ = n1 #+ natToSNat (undefined :: n)

type family a + b :: * where
  N0 + b       = b
  -- Add a N0       = a
  (Succ a) + b = Succ (a + b)
infixl 6 +

type family a - b :: * where
  N0 - b               = N0
  a - N0               = a
  (Succ a) - (Succ b)  = a - b

type family Min a b :: * where
  Min N0 m              = N0
  Min n N0              = N0
  Min (Succ n) (Succ m) = Succ(Min n m)

-- O, I, X mean 0, 1, x respectively
data Bit = O | I | X deriving Eq
instance Show Bit where
  show O = "0"
  show I = "1"
  show X = "x"

-- Bit NOT
inv O = I
inv I = O
inv X = X

-- Bits synonim can be changed, so I use the synonim
data Bits :: * -> * where
  End :: Bits N0
  (:*) :: Bit -> Bits n -> Bits (Succ n)

infixr 0 :*
deriving instance Eq (Bits n)

instance Show (Bits n) where
  show End     = ""
  show (b:*bs) = show b ++ show bs

dropBits :: SNat n -> Bits m -> Bits (m - n)
dropBits SN0 bits           = bits
dropBits _   End            = End　-- Caustion: N0 - N5 = N0
dropBits (SSucc n) (_:*bs)  = dropBits n bs

takeBits :: SNat n -> Bits m -> Bits (Min n m)
takeBits SN0 bits           = End
takeBits _   End            = End
takeBits (SSucc n) (b:*bs)  = b :* (takeBits n bs)

headBits :: Bits (Succ n) -> Bit
headBits (b:*bs) = b

-- fold left for Bits
foldlBits :: (a -> Bit -> a) -> a -> Bits n -> a
foldlBits op zero End     = zero
foldlBits op zero (x:*xs) = foldlBits op (op zero x) xs

-- fold right for Bits
foldrBits :: (Bit -> a -> a) -> a -> Bits n -> a
foldrBits op zero End     = zero
foldrBits op zero (x:*xs) = op x (foldrBits op zero xs)

-- fold left Maybe for Bits
foldlMaybeBits :: (a -> Bit -> Maybe a) -> a -> Bits n -> Maybe a
foldlMaybeBits op zero End     = Just zero
foldlMaybeBits op zero (x:*xs) = do
  a <- op zero x
  foldlMaybeBits op a xs

lengthBits :: Bits n -> SNat n
lengthBits End     = n0
lengthBits (b:*bs) = n1 #+ lengthBits bs

range :: SNat s -> SNat e -> Bits n -> Bits ( Min (s - e + N1) (n - (n - N1 - s)) )
range s e bs = takeBits (s #- e #+ n1) . dropBits (n #- n1 #- s) $ bs
  where n = lengthBits bs

-- Convert Bits into [Bit]
bitsToList :: Bits n -> [Bit]
bitsToList End     = []
bitsToList (x:*xs) = x : bitsToList xs

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
I #| _ = I
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

-- NOR gate
norGate :: SF (Bit, Bit) Bit
norGate = proc (a, b) -> do
  oro <- orGate -< (a, b)
  invGate -< oro

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
add4Bits :: SF (Bits N4, Bits N4) (Bits N4)
-- 残念ながら、procとGATDsを両方使った状態でパターンマッチできない（Proc patterns cannot use existential or GADT data constructors）
-- add4Bits = proc (a3:*a2:*a1:*a0:*End, b3:*b2:*b1:*b0:*End) -> do
add4Bits = proc (bitsToList -> [a3,a2,a1,a0], bitsToList -> [b3,b2,b1,b0]) -> do -- ViewPatternを使って上記の問題をしのぐ。これではビット数でのパターンマッチエラーをコンパイル時に防げない。残念
  (c0, s0) <- halfAdder -< (a0, b0)
  (c1, s1) <- fullAdder -< (a1, b1, c0)
  (c2, s2) <- fullAdder -< (a2, b2, c1)
  (c3, s3) <- fullAdder -< (a3, b3, c2)
  returnA -< s3:*s2:*s1:*s0:*End

-- 4 bits Substractor
sub4Bits :: SF (Bits N4, Bits N4) (Bits N4)
sub4Bits = proc (bitsToList -> [a3,a2,a1,a0], bitsToList -> [b3,b2,b1,b0]) -> do -- ViewPatternを使って上記の問題をしのぐ。これではビット数でのパターンマッチエラーをコンパイル時に防げない。残念
  (c0, s0) <- fullAdder -< (a0, inv b0, I)
  (c1, s1) <- fullAdder -< (a1, inv b1, c0)
  (c2, s2) <- fullAdder -< (a2, inv b2, c1)
  (c3, s3) <- fullAdder -< (a3, inv b3, c2)
  returnA -< s3:*s2:*s1:*s0:*End

-- AND - 4 Bits
and4Bits :: SF (Bits N4, Bits N4) (Bits N4)
and4Bits = proc (bitsToList -> [a3,a2,a1,a0], bitsToList -> [b3,b2,b1,b0]) -> do
  o0 <- andGate -< (a0, b0)
  o1 <- andGate -< (a1, b1)
  o2 <- andGate -< (a2, b2)
  o3 <- andGate -< (a3, b3)
  returnA -< o3:*o2:*o1:*o0:*End

-- OR - 4 Bits
or4Bits :: SF (Bits N4, Bits N4) (Bits N4)
or4Bits = proc (bitsToList -> [a3,a2,a1,a0], bitsToList -> [b3,b2,b1,b0]) -> do
  o0 <- orGate -< (a0, b0)
  o1 <- orGate -< (a1, b1)
  o2 <- orGate -< (a2, b2)
  o3 <- orGate -< (a3, b3)
  returnA -< o3:*o2:*o1:*o0:*End


-- Set less than -- 4 Bits
-- if as < bs then 1 else 0 (as and bs are signed values)
lt4Bits :: SF (Bits N4, Bits N4) (Bits N4)
lt4Bits = proc (as, bs) -> do
  subo <- sub4Bits -< (as, bs)
  let neg = headBits subo
  returnA -< O:*O:*O:*neg:*End

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

-- -- RS Flip-Flop -- NOR Base
-- rsff :: SF (Bit, Bit) (Bit, Bit, (Bit, Bit))
-- rsff = proc (s, r) -> do
--   rec
--     preQ  <- dHold O -< Event q
--     preQ_ <- dHold I -< Event q_
--     q  <- norGate -< (s, preQ_)
--     q_ <- norGate -< (r, preQ)
--   returnA -< (q, q_, (preQ, preQ_))

-- Converter for Bits to Int number
bitsToIntMaybe :: Bits a -> Maybe Int
bitsToIntMaybe bs = foldlMaybeBits (\s b -> case b of
    O -> Just $ s `shift` 1 .|. 0
    I -> Just $ s `shift` 1 .|. 1
    X -> Nothing
  ) 0 bs

-- -- Converter for [Int] to Bits
-- toBits :: [Int] -> Bits a
-- toBits = Bits $ fmap (\e -> if e == 1 then I else O)


-- -- TODO use range(but not implemented)
-- bitsDrop n (Bits bs) = Bits $ drop n bs

-- empty 4 bit memory
empty4BitsMem :: Bits N4
empty4BitsMem = O:*O:*O:*O:*End

-- -- rewrite all memry
-- rewriteMem :: Bits N4 -> Bits N4 -> Bits N4
-- rewriteMem bits _ = bits

-- メモリとしての機能を果たすか作ってみて確かめる
memTest :: SF (Bits N4, Bit) (Bits N4)
memTest = proc (input, writeFlag) -> do
  output <- dHold empty4BitsMem -< if writeFlag == I then Event input else NoEvent
  returnA -<  output

  -- -- empty 4 bit memory
  -- empty4BitsMem :: Bits N4
  -- empty4BitsMem = Bits [O,O,O,O]
  --
  -- -- -- rewrite all memry
  -- -- rewriteMem :: Bits N4 -> Bits N4 -> Bits N4
  -- -- rewriteMem bits _ = bits
  --
  -- -- メモリとしての機能を果たすか作ってみて確かめる
  -- memTest :: SF (Bits N4, Bit) (Bits N4)
  -- memTest = proc (input, writeFlag) -> do
  --   output <- dHold empty4BitsMem -< if writeFlag == I then Event input else NoEvent
  --   returnA -<  output

-- Test for 4 bits adder
testForAdd4bits = do
  prevOut <- newIORef (undefined :: Bits N4)
  let zero = O:*O:*O:*O:*End
      one  = O:*O:*O:*I:*End
  reactimate (return (zero, zero))
               (\_ -> do
                 threadDelay 100000
                 out <- readIORef prevOut
                 return (0.1, (Just $ (out, one) )))
               (\_ out -> print (out, bitsToIntMaybe out) >> writeIORef prevOut out >> return False)
               (add4Bits)

-- Test for 4 bits sub
testForSub4bits = do
 prevOut <- newIORef (undefined :: Bits N4)
 let
     zero = O:*O:*O:*O:*End
     b15 = I:*I:*I:*I:*End
     one  = O:*O:*O:*I:*End
     a = embed (sub4Bits) ((b15, zero), [])
 reactimate (return (b15, zero))
              (\_ -> do
                threadDelay 100000
                out <- readIORef prevOut
                return (0.1, (Just $ (out, one) )))
              (\_ out -> print (out, bitsToIntMaybe out) >> writeIORef prevOut out >> return False)
              (sub4Bits)

-- Test for 4 bits and
testForAnd4bits = do
 prevOut <- newIORef (undefined :: Bits N4)
 let
     zero = O:*O:*O:*O:*End
     b15 = I:*I:*I:*I:*End
     one  = O:*O:*O:*I:*End
     a = embed (sub4Bits) ((b15, zero), [])
 reactimate (return (b15, zero))
              (\_ -> do
                threadDelay 100000
                out <- readIORef prevOut
                return (0.1, (Just $ (out, one) )))
              (\_ out -> print (out, bitsToIntMaybe out) >> writeIORef prevOut out >> return False)
              (and4Bits)

-- Test for 4 bits adder and Memory
testForAdd4bitsAndMem = do
 let zero = O:*O:*O:*O:*End
     one  = O:*O:*O:*I:*End
     mainSF :: SF Bit (Bits N4)
     mainSF = proc writeFlag -> do
      rec
        memData <- memTest  -< (added, writeFlag)
        added   <- add4Bits -< (memData, one)
      returnA -< added

 reactimate (return O)
            (\_ -> threadDelay 100000 >> return (0.1, Just I))
            (\_ out -> print (out, bitsToIntMaybe out) >> return False)
            (mainSF)

-- Return Time delayed SF
delayedSF :: Eq b => Time -> b -> SF a b -> SF a b
delayedSF time init sf = proc sfIn -> do
  rec
    sfOut   <- sf -< sfIn
    pre     <- dHold init -< Event out
    delayed <- delay time init -< sfOut
    let out = if delayed == init then pre else delayed
  returnA -< out

-- Return filled bits with bit
fillBits :: Bit -> SNat n -> Bits n
fillBits bit SN0       = End
fillBits bit (SSucc n) = bit :* fillBits bit n

-- n bits X
unknowns :: SNat n -> Bits n
unknowns = fillBits X

-- Test for delayed 4 bits adder and Memory
testForDelayAdd4bitsAndMem = do
 let zero = O:*O:*O:*O:*End
     one  = O:*O:*O:*I:*End
     mainSF :: SF Bit (Bits N4)
     mainSF = proc writeFlag -> do
      rec
        memData  <- memTest  -< (out, writeFlag)
        out    <- delayedSF 0.5 (unknowns n4) add4Bits -< (memData, one)
      returnA -< out

 reactimate (return O)
            (\_ -> threadDelay 100000 >> return (0.1, Just I))
            (\_ out -> print (out, bitsToIntMaybe out) >> return False)
            (mainSF)

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
    (rsff) -- 使いたいSF
    (reset, [(0.1, Just e) | e <- [reset, hold, set, set]])--, set, hold, set, reset, reset, hold, hold, hold] ])

orTest = do
  print $ I #| undefined -- I
  print $ X #| I         -- I

main :: IO ()
main = do
  print $ fillBits O n5
  print $ unknowns n6
