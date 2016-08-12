{-# LANGUAGE Arrows        #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module AppliedUnit(
  register,
  registerInit,
  alu,
  mux5In2,
  mux32In2,
  mux32In5,
  mainControl,
  aluControl,
  signExt,
  shiftLeft2,
  iMem,
  dataMem,
  progCounter
) where


import           BasicUnit
import           Bit
import           Data.Maybe
import           FRP.Yampa
import           Natural
import           Util


-- Register
register :: SF (Bits N5, Bits N5, Bits N5, Bits N32, Bit, Bit, Bit) (Bits N32, Bits N32)
register = registerInit (replicate 32 (fillBits O n32))

-- Register with initialization
-- TODO クリアでの挙動を実装していない
-- TODO initをただのリストではなく長さ付きのリストにする（より型安全に）
registerInit :: [Bits N32] -> SF (Bits N5, Bits N5, Bits N5, Bits N32, Bit, Bit, Bit) (Bits N32, Bits N32)
registerInit init = proc (readAddr1, readAddr2, writeAddr, writeData, clk, clr, writeFlag) -> do
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
  stored <- dAccumHold init -< storeEv
  -- Read Addresses to Indexies
  let read1Idx = fromMaybe 0 (bitsToIntMaybe readAddr1)
      read2Idx = fromMaybe 0 (bitsToIntMaybe readAddr2)
  -- Outputs are read data
  returnA -< (stored !! read1Idx, stored !! read2Idx)



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

-- Multiplexer (1 in 2, output is 32bits)
mux32In2 :: SF (Bits N32, Bits N32, Bit) (Bits N32)
mux32In2 = proc (o, i, d) -> do
  returnA -< case d of
    I -> i
    _ -> o



-- Multiplexer (1 in 2, output is 5bits)
mux5In2 :: SF (Bits N5, Bits N5, Bit) (Bits N5)
mux5In2 = proc (o, i, d) -> do
  returnA -< case d of
    I -> i
    _ -> o

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
  aluControlFunc (op, funct, aluOp) = case aluOp of
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
      I:*I:*End -> case op of
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

-- Shift left 2 (x4)
shiftLeft2 :: SF (Bits N32) (Bits N32)
shiftLeft2 = arr shiftLeft2Func
  where shiftLeft2Func :: Bits N32 -> Bits N32
        shiftLeft2Func din = dropBits n2 din +*+ (O:*O:*End)

-- Instraction Memory
iMem :: [Bits N32] -> SF (Bits N32) (Bits N32)
iMem init = proc addr -> do
  -- address to index
  let idx = fromMaybe 0 (bitsToIntMaybe (dropBits n16 addr))
  returnA -< (init !! (idx `div` 4))

-- Data Memory
dataMem :: SF (Bits N32, Bits N32, Bit, Bit, Bit) (Bits N32, [Bits N32])
dataMem = proc (addr, writeData, memWrite, memRead, clk) -> do
  negClk <- edge -< clk == O
  let index = fromMaybe 0 (bitsToIntMaybe addr)
      a = if memWrite == I then Event (\st -> listUpdate st (index `div` 4) writeData) else NoEvent
  stored <- dAccumHold (replicate 65535 (fillBits O n32)) -< negClk `goOnRight` a
  returnA -< (if memRead == I then stored !! (index `div` 4) else fillBits X n32, stored)

-- Program Counter
-- TODO クリアの実装
-- TODO クロックが立ち下がり瞬間は古いPCが出力される（対処できるのか？しなくてもいいかの？）
progCounter :: SF (Bits N32, Bit, Bit) (Bits N32)
progCounter = proc (nextPc, clk, clr) -> do
  -- Negative edge Clock
  negClk <- edge -< clk == O
  nowPc  <- dHold (fillBits O n32) -< negClk `tag` nextPc
  returnA -< nowPc

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
