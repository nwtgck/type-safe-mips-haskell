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
  lt4Bits,
  add32Bits
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

-- 32bits Adder
add32Bits :: SF (Bits N32, Bits N32) (Bits N32)
add32Bits = proc (bitsToList -> [a31,a30,a29,a28,a27,a26,a25,a24,a23,a22,a21,a20,a19,a18,a17,a16,a15,a14,a13,a12,a11,a10,a9,a8,a7,a6,a5,a4,a3,a2,a1,a0], bitsToList -> [b31,b30,b29,b28,b27,b26,b25,b24,b23,b22,b21,b20,b19,b18,b17,b16,b15,b14,b13,b12,b11,b10,b9,b8,b7,b6,b5,b4,b3,b2,b1,b0]) -> do
  (c0, s0)   <- halfAdder -< (a0, b0)
  (c1, s1)   <- fullAdder -< (a1, b1, c0)
  (c2, s2)   <- fullAdder -< (a2, b2, c1)
  (c3, s3)   <- fullAdder -< (a3, b3, c2)
  (c4, s4)   <- fullAdder -< (a4, b4, c3)
  (c5, s5)   <- fullAdder -< (a5, b5, c4)
  (c6, s6)   <- fullAdder -< (a6, b6, c5)
  (c7, s7)   <- fullAdder -< (a7, b7, c6)
  (c8, s8)   <- fullAdder -< (a8, b8, c7)
  (c9, s9)   <- fullAdder -< (a9, b9, c8)
  (c10, s10) <- fullAdder -< (a10, b10, c9)
  (c11, s11) <- fullAdder -< (a11, b11, c10)
  (c12, s12) <- fullAdder -< (a12, b12, c11)
  (c13, s13) <- fullAdder -< (a13, b13, c12)
  (c14, s14) <- fullAdder -< (a14, b14, c13)
  (c15, s15) <- fullAdder -< (a15, b15, c14)
  (c16, s16) <- fullAdder -< (a16, b16, c15)
  (c17, s17) <- fullAdder -< (a17, b17, c16)
  (c18, s18) <- fullAdder -< (a18, b18, c17)
  (c19, s19) <- fullAdder -< (a19, b19, c18)
  (c20, s20) <- fullAdder -< (a20, b20, c19)
  (c21, s21) <- fullAdder -< (a21, b21, c20)
  (c22, s22) <- fullAdder -< (a22, b22, c21)
  (c23, s23) <- fullAdder -< (a23, b23, c22)
  (c24, s24) <- fullAdder -< (a24, b24, c23)
  (c25, s25) <- fullAdder -< (a25, b25, c24)
  (c26, s26) <- fullAdder -< (a26, b26, c25)
  (c27, s27) <- fullAdder -< (a27, b27, c26)
  (c28, s28) <- fullAdder -< (a28, b28, c27)
  (c29, s29) <- fullAdder -< (a29, b29, c28)
  (c30, s30) <- fullAdder -< (a30, b30, c29)
  (c31, s31) <- fullAdder -< (a31, b31, c30)
  returnA -< s31:*s30:*s29:*s28:*s27:*s26:*s25:*s24:*s23:*s22:*s21:*s20:*s19:*s18:*s17:*s16:*s15:*s14:*s13:*s12:*s11:*s10:*s9:*s8:*s7:*s6:*s5:*s4:*s3:*s2:*s1:*s0:*End

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
