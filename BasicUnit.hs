{-# LANGUAGE Arrows             #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE ViewPatterns       #-}

module BasicUnit(
  andGate,
  orGate,
  xorGate,
  invGate,
  nandGate,
  norGate,
  halfAdder,
  fullAdder,
  add4Bits,
  sub4Bits,
  and4Bits,
  or4Bits,
  lt4Bits
) where


import           Bit
import           FRP.Yampa
import           Natural

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
