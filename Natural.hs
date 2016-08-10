{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Natural(
  N0,Succ,
  N1,N2,N3,N4,N5,
  SNat(..),
  n0,n1,n2,n3,n4,n5,n6,n7,
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
