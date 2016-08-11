{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Natural(
  N0,Succ,
  N1,N2,N3,N4,N5,N6,N7,N8,N9,N10,N11,N12,N13,N14,N15,N16,N17,N18,N19,N20,N21,N22,N23,N24,N25,N26,N27,N28,N29,N30,N31,N32,
  SNat(..),
  n0,n1,n2,n3,n4,n5,n6,n7,n8,n9,n10,n11,n12,n13,n14,n15,n16,n17,n18,n19,n20,n21,n22,n23,n24,n25,n26,n27,n28,n29,n30,n31,n32,
  (#+), (#-),
  type(+), type(-),
  Min
) where

data N0
data Succ n
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4
type N6 = Succ N5
type N7 = Succ N6
type N8 = Succ N7
type N9 = Succ N8
type N10 = Succ N9
type N11 = Succ N10
type N12 = Succ N11
type N13 = Succ N12
type N14 = Succ N13
type N15 = Succ N14
type N16 = Succ N15
type N17 = Succ N16
type N18 = Succ N17
type N19 = Succ N18
type N20 = Succ N19
type N21 = Succ N20
type N22 = Succ N21
type N23 = Succ N22
type N24 = Succ N23
type N25 = Succ N24
type N26 = Succ N25
type N27 = Succ N26
type N28 = Succ N27
type N29 = Succ N28
type N30 = Succ N29
type N31 = Succ N30
type N32 = Succ N31

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
n8 = SSucc n7
n9 = SSucc n8
n10 = SSucc n9
n11 = SSucc n10
n12 = SSucc n11
n13 = SSucc n12
n14 = SSucc n13
n15 = SSucc n14
n16 = SSucc n15
n17 = SSucc n16
n18 = SSucc n17
n19 = SSucc n18
n20 = SSucc n19
n21 = SSucc n20
n22 = SSucc n21
n23 = SSucc n22
n24 = SSucc n23
n25 = SSucc n24
n26 = SSucc n25
n27 = SSucc n26
n28 = SSucc n27
n29 = SSucc n28
n30 = SSucc n29
n31 = SSucc n30
n32 = SSucc n31


type family a + b :: * where
  N0 + b       = b
  -- Add a N0       = a
  (Succ a) + b = Succ (a + b)
infixl 6 +

type family a - b :: * where
  N0 - b               = N0
  a - N0               = a
  (Succ a) - (Succ b)  = a - b
infixl 6 -

type family Min a b :: * where
  Min N0 m              = N0
  Min n N0              = N0
  Min (Succ n) (Succ m) = Succ(Min n m)

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
