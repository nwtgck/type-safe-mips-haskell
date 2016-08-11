-- Yampaを使って論理回路を作る

{-# LANGUAGE Arrows        #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}


import           BasicUnit
import           Bit
import           Control.Concurrent
import           Data.IORef
import           Data.Maybe
import           DelayedSF
import           FRP.Yampa
import           Natural

-- メモリとしての機能を果たすか作ってみて確かめる
memTest :: SF (Bits N4, Bit) (Bits N4)
memTest = proc (input, writeFlag) -> do
  output <- dHold (fillBits O n4) -< if writeFlag == I then Event input else NoEvent
  returnA -<  output

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

-- Test for 32 bits adder
testForAdd32bits = do
 prevOut <- newIORef (undefined :: Bits N32)
 let zero = fillBits O n32
     one  = O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*I:*End
 reactimate (return (zero, zero))
              (\_ -> do
                threadDelay 80000
                out <- readIORef prevOut
                return (0.1, (Just $ (out, one) )))
              (\_ out -> print (out, bitsToIntMaybe out) >> writeIORef prevOut out >> return False)
              (add32Bits)

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

-- Update a List by specific index
listUpdate :: [a] -> Int -> a -> [a]
listUpdate []     _   _ = error "Can't update list"
listUpdate (x:xs) 0   v = v : xs
listUpdate (x:xs) idx v = x: listUpdate xs (idx-1) v


-- If left is Event a, go on to Right Event
goOnRight :: Event a -> Event b -> Event b
goOnRight NoEvent _ = NoEvent
goOnRight _       r = r

-- Resister
-- TODO クリアでの挙動を実装していない
resister :: SF (Bits N4, Bits N4, Bits N4, Bits N32, Bit, Bit, Bit) (Bits N32, Bits N32)
resister = proc (readAddr1, readAddr2, writeAddr, writeData, clk, clr, writeFlag) -> do
  -- negative edge for clock
  negClkEv <- edge -< clk == O
  let
    -- Parse write address (if Nothing: 0)
    writeIdx = fromMaybe 0 (bitsToIntMaybe writeAddr)
    writeEv = if writeFlag == I && writeIdx /= 0 -- $0 is always 0
              then Event (\stored -> listUpdate stored writeIdx writeData)
              else NoEvent
    storeEv = negClkEv `goOnRight` writeEv
  -- Get stored data
  stored <- accumHold (replicate 32 (fillBits O n32)) -< storeEv
  -- Read Addresses to Indexies
  let read1Idx = fromMaybe 0 (bitsToIntMaybe readAddr1)
      read2Idx = fromMaybe 0 (bitsToIntMaybe readAddr2)
  -- Outputs are read data
  returnA -< (stored !! read1Idx, stored !! read2Idx)


-- レジスタのテスト
testForResister = do
  -- let xs = [1, 2, 3, 4, 5]
  -- print $ listUpdate xs 4 10000

  let
      -- 4bitの0, 1
      b0 = fillBits O n4
      b1 = O:*O:*O:*I:*End
      b5 = O:*I:*O:*I:*End

      -- 書き込み使うデータ(32bitすべてで全部Iが詰まってるものと11)
      writeData1 = fillBits I n32
      writeData2 = O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*I:*O:*I:*I:*End

      clockup = (b0, b0, b0, writeData1, I, X, O) -- クロックを立ち上げる
      s0 = clockup -- クロックを立ち上げる
      s1 = (b0, b5, b0, writeData1, O, X, O) -- 0,5番目を読みだすだけ（書き込みはしない）--クロックをたち下げ
      s2 = clockup -- クロックを立ち上げる
      s3 = (b0, b5, b5, writeData1, O, X, I) -- 0,5番目を読みだし、書き込む
      s4 = clockup -- クロックを立ち上げる
      s5 = (b0, b5, b5, writeData2, O, X, I) -- 0,5番目を読みだし、書き込む
      s6 = (b0, b5, b5, writeData1, O, X, I) -- 0,5番目を読みだし、書き込む -- (ただクロックの立ち下がりではないため書き込まれない)


  print $ embed
    (resister) -- 使いたいSF
    (s0, [(0.1, Just e) | e <- [s1, s2, s3, s4, s5, s6]])

    -- 出力
    -- [(00000000000000000000000000000000,00000000000000000000000000000000),(00000000000000000000000000000000,00000000000000000000000000000000),(00000000000000000000000000000000,00000000000000000000000000000000),(00000000000000000000000000000000,11111111111111111111111111111111),(00000000000000000000000000000000,00000000000000000000000000000000),(00000000000000000000000000000000,00000000000000000000000000001011),(00000000000000000000000000000000,00000000000000000000000000001011)]

-- -- alu
-- alu :: SF (Bits N32, Bits N32, Bits N3) (Bits N32)
-- alu = arr aluFunc
--   where
--     aluFunc a b aluOp =
--       let func = case alOp of
--         (O:*O:*O:*End) -> add4Bits

main :: IO ()
main = testForAdd32bits
