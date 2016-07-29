-- Yampaを使って論理回路を作る
-- TODO Bitsに長さを含めて型付けする（ビット違いの配線ミスを防ぐため）

{-# LANGUAGE Arrows #-}

import           Control.Concurrent
import           Control.Monad
import           Data.Bits          (shift, (.|.))
import           Data.IORef
import           Data.Maybe
import           FRP.Yampa

-- O, I, X mean 0, 1, x respectively
data Bit = O | I | X deriving Show

type Bits = [Bit] -- Bits synonim can be changed, so I use the synonim

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
add4Bits :: SF (Bits, Bits) Bits
add4Bits = proc ([a3,a2,a1,a0], [b3,b2,b1,b0]) -> do
  (c0, s0) <- halfAdder -< (a0, b0)
  (c1, s1) <- fullAdder -< (a1, b1, c0)
  (c2, s2) <- fullAdder -< (a2, b2, c1)
  (c3, s3) <- fullAdder -< (a3, b3, c2)
  returnA -< [c3,s3,s2,s1,s0]

-- Converter for Bits to Int number
bitsToIntMay :: Bits -> Maybe Int
bitsToIntMay = foldM (\s b -> case b of
    O -> Just $ s `shift` 1 .|. 0
    I -> Just $ s `shift` 1 .|. 1
    X  -> Nothing
  ) 0

-- Converter for [Int] to Bits
toBits :: [Int] -> Bits
toBits = fmap (\e -> if e == 1 then I else O)


-- Test for 4 bits adder
testForAdd4bits = do
  prevOut <- newIORef ([] :: Bits)
  let zero = toBits [0,0,0,0]
      one  = toBits [0,0,0,1]
  reactimate (return (zero, zero))
               (\_ -> do
                 threadDelay 100000
                 out <- readIORef prevOut
                 return (0.1, (Just $ (drop 1 out, one) )))
               (\_ out -> print (out, bitsToIntMay out) >> writeIORef prevOut out >> return False)
               (add4Bits)

main :: IO ()
main = testForAdd4bits
