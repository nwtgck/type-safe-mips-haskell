-- Yampaを使って論理回路を作る
-- TODO Bitsに長さを含めて型付けする（ビット違いの配線ミスを防ぐため）(多少型付けした)

{-# LANGUAGE Arrows              #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Concurrent
import           Control.Monad
import           Data.Bits          (shift, (.|.))
import           Data.IORef
import           Data.Maybe
import           FRP.Yampa
import           Unsafe.Coerce


data N0
data Succ n

class Nat n where
  num :: n -> Int

instance Nat N0 where
  num _ = 0

instance Nat n => Nat (Succ n) where
  num _ = 1 + (num (undefined :: n))

type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4

-- 足し算で型を生成する(Add One Two型はThree型)
type family Add a b :: * where
  Add n N0           = n                     -- a add 0 = a
  Add N0 n           = n                     -- 0 add b = b
  Add (Succ a) (Succ b) = Succ(Succ (Add a b)) -- (a +1) add (b +1) = (a add b) +1 +1
--
type family Range s n :: * where
  Range n N0 = Bits n
  Range N0 n = Bits n
  Range (Succ a) (Succ b) = Bits (Add a b)

-- O, I, X mean 0, 1, x respectively
data Bit = O | I | X deriving Show

data Bits n = Bits [Bit] deriving Show -- Bits synonim can be changed, so I use the synonim

-- Bit OR
O #| O = O
X #| _ = X
_ #| X = X
_ #| _ = I

-- Bit AND
I #& I = I
X #& _ = X
_ #& X = X
_ #& _ = O

-- Bit XOR
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

-- Converter for Bits to Int number
bitsToIntMay :: Bits a -> Maybe Int
bitsToIntMay (Bits bs) = foldM (\s b -> case b of
    O -> Just $ s `shift` 1 .|. 0
    I -> Just $ s `shift` 1 .|. 1
    X  -> Nothing
  ) 0 bs

rangeA :: (Nat n, Nat s, Nat e) => Bits n -> Range s e
rangeA (Bits bs) =
  let nnum = num (undefined :: n)
      snum = num (undefined :: s)
      enum = num (undefined :: e)
  in (unsafeCoerce (Bits . take (snum-enum+1) . drop (nnum-1-snum) $ bs) :: Range s e)

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

main :: IO ()
main = testForAdd4bits
