{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Bit(
  Bit(..),
  Bits(..),
  (#&),
  (#|),
  inv,
  (#^),
  dropBits,
  takeBits,
  headBits,
  (+*+),
  foldlBits,
  foldrBits,
  foldlMaybeBits,
  lengthBits,
  range,
  bitsToList,
  fillBits,
  unknowns,
  bitsToIntMaybe
) where

import           Data.Bits (shift, (.|.))
import           Natural


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

-- append bits
(+*+) :: Bits n -> Bits m -> Bits (n+m)
End     +*+ bs = bs
(x:*xs) +*+ bs = x :* (xs +*+ bs)
infixr 5 +*+

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

-- Return filled bits with bit
fillBits :: Bit -> SNat n -> Bits n
fillBits bit SN0       = End
fillBits bit (SSucc n) = bit :* fillBits bit n

-- n bits X
unknowns :: SNat n -> Bits n
unknowns = fillBits X

-- Converter for Bits to Int number
bitsToIntMaybe :: Bits a -> Maybe Int
bitsToIntMaybe bs = foldlMaybeBits (\s b -> case b of
    O -> Just $ s `shift` 1 .|. 0
    I -> Just $ s `shift` 1 .|. 1
    X -> Nothing
  ) 0 bs
