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
                -- threadDelay 80000
                out <- readIORef prevOut
                return (0.1, (Just $ (out, one) )))
              (\_ out -> print (out, bitsToIntMaybe out) >> writeIORef prevOut out >> return False)
              (add32Bits)

-- Test for 32 bits sub
testForSub32bits = do
 prevOut <- newIORef (undefined :: Bits N32)
 let
     zero = fillBits O n32
     maxB = fillBits I n32
     one  = O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*O:*I:*End
 reactimate (return (maxB, zero))
              (\_ -> do
                -- threadDelay 80000
                out <- readIORef prevOut
                return (0.1, (Just $ (out, one) )))
              (\_ out -> print (out, bitsToIntMaybe out) >> writeIORef prevOut out >> return False)
              (sub32Bits)

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

-- ALU
alu :: SF (Bits N32, Bits N32, Bits N3) (Bits N32, Bit)
alu = proc (a, b, oper) -> do
  andRes <- and32Bits -< (a, b)
  orRes  <- or32Bits  -< (a, b)
  addRes <- add32Bits -< (a, b)
  subRes <- sub32Bits -< (a, b)
  let ltRes = fillBits O n31 +*+ takeBits n1 subRes
  res <- mux32In5 -< (andRes, orRes, addRes, subRes, ltRes, oper)
  let zeroFlag = if res == fillBits O n32 then I else O
  returnA -< (res, zeroFlag)

-- Multiplexer (1 in 4, output is 32bits)
mux32In5 :: SF (Bits N32, Bits N32, Bits N32, Bits N32, Bits N32, Bits N3) (Bits N32)
mux32In5 = proc (ooo, ooi, oio, iio, iii, d) -> do
  returnA -< case d of
    O:*O:*I:*End -> ooi
    O:*I:*O:*End -> oio
    I:*I:*O:*End -> iio
    I:*I:*I:*End -> iii
    _            -> ooo

-- ALUのテスト
testForAlu :: IO ()
testForAlu = do
  let
      -- 32bits values
      b0 = fillBits O n32                       :: Bits N32
      b1 = fillBits O n31 +*+ (I:*End)          :: Bits N32
      b5 = fillBits O n28 +*+ (O:*I:*O:*I:*End) :: Bits N32

      -- ALUOp
      cAND = O:*O:*O:*End
      cOR  = O:*O:*I:*End
      cADD = O:*I:*O:*End
      cSUB = I:*I:*O:*End
      cLt  = I:*I:*I:*End

  return ()
  print $ embed
    (alu) -- 使いたいSF
    ((b5, b5, cADD), [(0.1, Just e) | e <- [(b1, b5, cAND), (b1, b5, cOR), (b1, b5, cSUB), (b1, b5, cLt), (b5, b1, cLt)]])

  -- 出力
  -- [00000000000000000000000000001010,00000000000000000000000000000001,00000000000000000000000000000101,11111111111111111111111111111100,00000000000000000000000000000001,00000000000000000000000000000000]

-- Main Control
mainControl :: SF (Bits N6) (Bit, Bit, Bit, Bit, Bits N2, Bit, Bit, Bit)
mainControl = arr mainControlFunc
  where
    mainControlFunc :: Bits N6 -> (Bit, Bit, Bit, Bit, Bits N2, Bit, Bit, Bit)
    -- R-format
    mainControlFunc (O:*O:*O:*O:*O:*O:*End) = (regDest, branch, memRead, memtoReg, aluOp, memWrite, aluSrc, regWrite)
      where regDest  = I
            aluSrc   = O
            memtoReg = O
            regWrite = I
            memRead  = O
            memWrite = O
            branch   = O
            aluOp    = I:*O:*End

    -- lw
    mainControlFunc (I:*O:*O:*O:*I:*I:*End) = (regDest, branch, memRead, memtoReg, aluOp, memWrite, aluSrc, regWrite)
      where regDest  = O
            aluSrc   = I
            memtoReg = I
            regWrite = I
            memRead  = I
            memWrite = O
            branch   = O
            aluOp    = O:*O:*End

    -- sw
    mainControlFunc (I:*O:*I:*O:*I:*I:*End) = (regDest, branch, memRead, memtoReg, aluOp, memWrite, aluSrc, regWrite)
      where regDest  = I
            aluSrc   = I
            memtoReg = I
            regWrite = O
            memRead  = O
            memWrite = I
            branch   = O
            aluOp    = O:*O:*End
    -- beq
    mainControlFunc (O:*O:*O:*I:*O:*O:*End) = (regDest, branch, memRead, memtoReg, aluOp, memWrite, aluSrc, regWrite)
      where regDest  = I
            aluSrc   = O
            memtoReg = I
            regWrite = O
            memRead  = O
            memWrite = O
            branch   = I
            aluOp    = O:*I:*End

    -- addi
    mainControlFunc (O:*O:*I:*O:*O:*O:*End) = (regDest, branch, memRead, memtoReg, aluOp, memWrite, aluSrc, regWrite)
      where regDest  = O
            aluSrc   = I
            memtoReg = O
            regWrite = I
            memRead  = O
            memWrite = O
            branch   = O
            aluOp    = I:*I:*End

    -- andi
    mainControlFunc (O:*O:*I:*I:*O:*O:*End) = (regDest, branch, memRead, memtoReg, aluOp, memWrite, aluSrc, regWrite)
      where regDest  = O
            aluSrc   = I
            memtoReg = O
            regWrite = I
            memRead  = O
            memWrite = O
            branch   = O
            aluOp    = I:*I:*End

    -- ori
    mainControlFunc (O:*O:*I:*I:*O:*I:*End) = (regDest, branch, memRead, memtoReg, aluOp, memWrite, aluSrc, regWrite)
      where regDest  = O
            aluSrc   = I
            memtoReg = O
            regWrite = I
            memRead  = O
            memWrite = O
            branch   = O
            aluOp    = I:*I:*End

    -- set less than immediate
    mainControlFunc (O:*O:*I:*O:*I:*O:*End) = (regDest, branch, memRead, memtoReg, aluOp, memWrite, aluSrc, regWrite)
      where regDest  = O
            aluSrc   = I
            memtoReg = O
            regWrite = I
            memRead  = O
            memWrite = O
            branch   = O
            aluOp    = I:*I:*End

-- ALU Control
aluControl :: SF (Bits N6, Bits N6, Bits N2) (Bits N3)
aluControl = arr aluControlFunc
 where
  aluControlFunc :: (Bits N6, Bits N6, Bits N2) -> Bits N3
  aluControlFunc (opcode, funct, aluOp) = case aluOp of
      O:*O:*End -> cAdd -- addition for lw or sw
      O:*I:*End -> cSub -- substraction for beq
      -- R-format
      I:*O:*End -> case funct of
        I:*O:*O:*O:*O:*O:*End -> cAdd
        I:*O:*O:*O:*I:*O:*End -> cSub
        I:*O:*O:*I:*O:*O:*End -> cAnd
        I:*O:*O:*I:*O:*I:*End -> cOr
        I:*O:*I:*O:*I:*O:*End -> cLt
      -- immediate
      I:*I:*End -> case opcode of
        O:*O:*I:*O:*O:*O:*End -> cAdd
        O:*O:*I:*I:*O:*O:*End -> cAnd
        O:*O:*I:*I:*O:*I:*End -> cOr
        O:*O:*I:*O:*I:*O:*End -> cLt
    where
      cAdd = O:*I:*O:*End
      cSub = I:*I:*O:*End
      cAnd = O:*O:*O:*End
      cOr  = O:*O:*I:*End
      cLt  = I:*I:*I:*End

-- Sign Extension
signExt :: SF (Bits N16) (Bits N32)
signExt = arr signExtFunc
  where signExtFunc :: Bits N16 -> Bits N32
        signExtFunc din = fillBits (headBits din) n16 +*+ din

main :: IO ()
main = testForAlu
