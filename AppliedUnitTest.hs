{-# LANGUAGE Arrows        #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}


import           AppliedUnit
import           BasicUnit
import           Bit
import           Data.Maybe
import           FRP.Yampa
import           Natural
import           Util

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

-- Testing for Instraction to ALU Result
-- 32bit命令からALUの結果までのテスト
testExecSF :: [Bits N32] -> SF (Bits N32) (Bits N32)
testExecSF regiInit = proc inst -> do

  -- Instraction devided by each meaning
  let op     = range n31 n26 inst
      rs     = range n25 n21 inst
      rt     = range n20 n16 inst
      rd     = range n15 n11 inst
      shamt  = range n10 n6  inst
      funct  = range n5  n0  inst
      offset = range n15 n0  inst

  -- Main Control
  (regDest, branch, memRead, memtoReg, aluOp, memWrite, aluSrc, regWrite) <- mainControl -< op

  -- Resigister
  writeAddr <- mux5In2 -< (rt, rd, regDest) -- Write Address
  (read1, read2) <- registerInit regiInit -< (rs, rt, writeAddr, fillBits O n32, O, O, O)

  -- ALU
  immediate             <- signExt    -< offset
  aluB                  <- mux32In2   -< (read2, immediate, aluSrc)
  oper                  <- aluControl -< (op, funct, aluOp)
  (aluResult, zeroFlag) <- alu        -< (read1, aluB, oper)

  returnA -< aluResult

-- 命令からALUまでのテスト
testForExecSF :: IO ()
testForExecSF = do
  let iAddi    = O:*O:*I:*O:*O:*O:*End :: Bits N6
      iOri     = O:*O:*I:*I:*O:*I:*End :: Bits N6
      regiInit = (replicate 32 (fillBits O n32))

      -- $0 = $0 + 4
      inst1 = iAddi +*+ (fillBits O n5) +*+ (fillBits O n5) +*+ (fillBits O n13 +*+ (I:*O:*O:*End)) :: Bits N32
      -- $0 = $0 or 11
      inst2 = iOri  +*+ (fillBits O n5) +*+ (fillBits O n5) +*+ (fillBits O n12 +*+ (I:*O:*I:*I:*End)) :: Bits N32
  print $ embed
    (testExecSF regiInit) -- 使いたいSF
    (inst1, [(0.1, Just e) | e <- [inst2]])

  return ()


-- PCのテスト
testForPc :: IO ()
testForPc = do
  let
    b4 = fillBits O n29 +*+ (I:*O:*O:*End) :: Bits N32
    sf :: SF Bit (Bits N32)
    sf = proc clk -> do
      rec
        pc     <- progCounter -< (nextPc, clk, O)
        nextPc <- add32Bits -< (pc, b4)
      returnA -< pc
    clocks = cycle [O, O, I, I]

  mapM_ (print . bitsToIntMaybe) $ embed
    (sf) -- 使いたいSF
    (O, [(0.1, Just e) | e <- clocks])
  return ()


-- レジスタのテスト
testForRegister = do
  let
      -- 5bitの0, 1
      b0 = fillBits O n5
      b1 = O:*O:*O:*O:*I:*End
      b5 = O:*O:*I:*O:*I:*End

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
    (register) -- 使いたいSF
    (s0, [(0.1, Just e) | e <- [s1, s2, s3, s4, s5, s6]])

    -- 出力
    -- [(00000000000000000000000000000000,00000000000000000000000000000000),(00000000000000000000000000000000,00000000000000000000000000000000),(00000000000000000000000000000000,00000000000000000000000000000000),(00000000000000000000000000000000,11111111111111111111111111111111),(00000000000000000000000000000000,00000000000000000000000000000000),(00000000000000000000000000000000,00000000000000000000000000001011),(00000000000000000000000000000000,00000000000000000000000000001011)]

main :: IO ()
main = testForExecSF
