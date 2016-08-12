-- Yampaを使ってMIPSアーキテクチャを作る

{-# LANGUAGE Arrows        #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}


import           AppliedUnit
import           BasicUnit
import           Bit
import           Control.Concurrent
import           FRP.Yampa
import           Natural
import           Text.Printf
import           Unsafe.Coerce


-- MIPS
mips :: [Bits N32] -> SF (Bit) (Bits N32, [Bits N32], Bits N32, Bits N32, Bits N32)
mips memInit = proc clk -> do
  rec
    -- Program Counter
    pc <- progCounter -< (nextPc, clk, O)

    -- Get Instraction
    inst <- iMem memInit -< pc

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
    (read1, read2) <- register -< (rs, rt, writeAddr, writeData, clk, O, regWrite)

    -- ALU
    immediate             <- signExt    -< offset
    aluB                  <- mux32In2   -< (read2, immediate, aluSrc)
    oper                  <- aluControl -< (op, funct, aluOp)
    (aluResult, zeroFlag) <- alu        -< (read1, aluB, oper)

    -- Data Memory
    (memData, allMemory) <- dataMem -< (aluResult, read2, memWrite, memRead, clk)

    -- Register Write Data
    writeData <- mux32In2 -< (aluResult, memData, memtoReg)

    -- Decide Branched PC
    plus4Pc        <- add32Bits  -< (pc, fillBits O n29 +*+ (I:*O:*O:*End))
    shiftedOffset  <- shiftLeft2 -< immediate
    branchedPc     <- add32Bits -< (plus4Pc, shiftedOffset)

    -- Decide next PC
    pcSrc  <- andGate  -< (branch, zeroFlag)
    nextPc <- mux32In2 -< (plus4Pc, branchedPc, pcSrc)

  returnA -< (pc, allMemory, writeData, inst, aluResult)

-- MIPSのテスト
mipsTest :: IO ()
mipsTest = do
  let
    iAddi = O:*O:*I:*O:*O:*O:*End :: Bits N6
    iOri  = O:*O:*I:*I:*O:*I:*End :: Bits N6
    iLw   = I:*O:*O:*O:*I:*I:*End
    iSw   = I:*O:*I:*O:*I:*I:*End
    iBeq  = O:*O:*O:*I:*O:*O:*End
    f     = I:*I:*I:*I:*End
    a     = I:*O:*I:*O:*End

    b4 = (O:*O:*I:*O:*O:*End)
    -- $4 = 2
    i1 = iAddi +*+ (fillBits O n5) +*+ b4 +*+ (fillBits O n14 +*+ (I:*O:*End)) :: Bits N32
    -- sw $4 h0000($0)
    i2 = iSw +*+ (fillBits O n5) +*+ b4 +*+ (fillBits O n16)

    -- PCを固定し、終了させないようにする命令
    foreverWait = iBeq +*+ (fillBits O n5) +*+ (fillBits O n5) +*+ (fillBits I n16)
    clocks = cycle [O, O, I, I]

    printFunc :: (Bits N32, [Bits N32], Bits N32, Bits N32, Bits N32) -> IO ()
    printFunc (pc, allMemory, writeData, inst, aluResult) = do
      printf "pc: %s, " (show $ bitsToIntMaybe pc)
      printf "memory[0]: %s, " (show (allMemory !! 0)) -- メモリの先頭だけを読み取る（計算結果を入れるつもり）
      printf "writeData: %s, " (show writeData)
      printf "inst: %s, " (show inst)
      printf "aluResult: %s\n" (show aluResult)
      threadDelay 100000

  mapM_ (printFunc) $ embed
    (mips [i1, i2, foreverWait]) -- 使いたいSF
    (O, [(0.1, Just e) | e <- clocks])



-- MIPSのテスト2
mipsTest2 :: IO ()
mipsTest2 = do
  let
    iRFormat = O:*O:*O:*O:*O:*O:*End :: Bits N6
    iAddi    = O:*O:*I:*O:*O:*O:*End :: Bits N6
    iOri     = O:*O:*I:*I:*O:*I:*End :: Bits N6
    iLw      = I:*O:*O:*O:*I:*I:*End
    iSw      = I:*O:*I:*O:*I:*I:*End
    iBeq     = O:*O:*O:*I:*O:*O:*End
    addFunct = I:*O:*O:*O:*O:*O:*End
    subFunct = I:*O:*O:*O:*I:*O:*End

    b0 = (O:*O:*O:*O:*O:*End)
    b1 = (O:*O:*O:*O:*I:*End)
    b2 = (O:*O:*O:*I:*O:*End)
    b3 = (O:*O:*O:*I:*I:*End)
    b4 = (O:*O:*I:*O:*O:*End)
    -- $1 = 2
    i1 = iAddi +*+ (fillBits O n5) +*+ b1 +*+ (fillBits O n14 +*+ (I:*O:*End)) :: Bits N32
    -- $2 = 3
    i2 = iAddi +*+ (fillBits O n5) +*+ b2 +*+ (fillBits O n14 +*+ (I:*I:*End)) :: Bits N32
    -- $3 = $1 + $2
    i3 = iRFormat +*+ b1 +*+ b2 +*+ b3 +*+ b0 +*+ addFunct :: Bits N32
    -- $4 = $2 - $1
    i4 = iRFormat +*+ b2 +*+ b1 +*+ b4 +*+ b0 +*+ subFunct :: Bits N32

    -- sw $4 h0000($0)
    i5 = iSw +*+ (fillBits O n5) +*+ b3 +*+ (fillBits O n16)
    -- sw $4 h0004($0)
    i6 = iSw +*+ (fillBits O n5) +*+ b4 +*+ (fillBits O n13 +*+ (I:*O:*O:*End))

    -- PCを固定し、終了させないようにする命令
    foreverWait = iBeq +*+ (fillBits O n5) +*+ (fillBits O n5) +*+ (fillBits I n16)
    clocks = cycle [O, O, I, I]

    printFunc :: (Bits N32, [Bits N32], Bits N32, Bits N32, Bits N32) -> IO ()
    printFunc (pc, allMemory, writeData, inst, aluResult) = do
      printf "pc: %s, " (show $ bitsToIntMaybe pc)
      printf "memory[0]: %s, " (show (allMemory !! 0)) -- メモリ0（$3の結果をメモリに入れたもの)
      printf "memory[1]: %s, " (show (allMemory !! 1)) -- メモリ1（$4の結果をメモリに入れたもの）
      printf "inst: %s, " (show inst)
      -- 長くなりすぎるのコメントアウトしてる
      -- printf "writeData: %s, " (show writeData)
      -- printf "aluResult: %s, " (show aluResult)
      putStrLn ""
      threadDelay 100000

  mapM_ (printFunc) $ embed
    (mips [i1, i2, i3, i4, i5, i6, foreverWait]) -- 使いたいSF
    (O, [(0.1, Just e) | e <- clocks])

-- Integer to Bits
-- TODO 負の数にも対応する
intToBits :: Int -> SNat n -> Bits n
intToBits _ SN0       = End
-- TODO unsafeCoerceを使わずに証明する
intToBits i (SSucc n) = unsafeCoerce $ (intToBits (i `div` 2) n) +*+ ((if i `mod` 2 == 0 then O else I):*End)

-- 1 ~ $3までの総和の計算（最終結果は$1に入り、memoryに書き込まれる）
mipsTest3 :: IO ()
mipsTest3 = do
  let
    iRFormat = O:*O:*O:*O:*O:*O:*End :: Bits N6
    iAddi    = O:*O:*I:*O:*O:*O:*End :: Bits N6
    iOri     = O:*O:*I:*I:*O:*I:*End :: Bits N6
    iLw      = I:*O:*O:*O:*I:*I:*End
    iSw      = I:*O:*I:*O:*I:*I:*End
    iBeq     = O:*O:*O:*I:*O:*O:*End
    addFunct = I:*O:*O:*O:*O:*O:*End
    subFunct = I:*O:*O:*O:*I:*O:*End
    sltFunct = I:*O:*I:*O:*I:*O:*End

    b0 = (O:*O:*O:*O:*O:*End)
    b1 = (O:*O:*O:*O:*I:*End)
    b2 = (O:*O:*O:*I:*O:*End)
    b3 = (O:*O:*O:*I:*I:*End)
    b4 = (O:*O:*I:*O:*O:*End)

    -- 1 ~ $3までの総和の計算（最終結果は$1に入り、memoryに書き込まれる）

    -- 0: $1 = 0
    i1 = iOri +*+ b0 +*+ b1 +*+ (intToBits 0 n16) :: Bits N32
    -- 4: $2 = 1
    i2 = iOri +*+ b0 +*+ b2 +*+ (intToBits 1 n16) :: Bits N32
    -- 8: $3 = 10
    i3 = iOri +*+ b0 +*+ b3 +*+ (intToBits 10 n16) :: Bits N32
    -- 12: add $1 $1 $2
    i4 = iRFormat +*+ b1 +*+ b2 +*+ b1 +*+ b0 +*+ addFunct :: Bits N32
    -- 16: addi $2 $2 1
    i5 = iAddi +*+ b2 +*+ b2 +*+ (intToBits 1 n16) :: Bits N32
    -- 20:slt $4 $3 $2
    i6 = iRFormat +*+ b3 +*+ b2 +*+ b4 +*+ b0 +*+ sltFunct :: Bits N32
    -- 24:beq $4 $0 (-4d)
    i7 = iBeq +*+ b4 +*+ b0 +*+ (fillBits I n13 +*+ (I:*O:*O:*End)) :: Bits N32
    -- 28:sw $4 h0004($0)
    i8 = iSw +*+ b0 +*+ b1 +*+ (fillBits O n16) :: Bits N32


    -- PCを固定し、終了させないようにする命令
    foreverWait = iBeq +*+ b0 +*+ b0 +*+ (fillBits I n16)
    clocks = cycle [O, O, I, I]

    printFunc :: (Bits N32, [Bits N32], Bits N32, Bits N32, Bits N32) -> IO ()
    printFunc (pc, allMemory, writeData, inst, aluResult) = do
      printf "pc: %s, " (show $ bitsToIntMaybe pc)
      printf "memory[0]: %s, " (show (bitsToIntMaybe $ allMemory !! 0)) -- メモリ0（$4(総和)の結果をメモリに入れたもの)
      -- printf "memory[1]: %s, " (show (allMemory !! 1)) -- メモリ1（$4の結果をメモリに入れたもの）
      printf "inst: %s, " (show inst)
      -- 長くなりすぎるのコメントアウトしてる
      printf "writeData: %s, " (show writeData)
      -- printf "aluResult: %s, " (show aluResult)
      putStrLn ""
      threadDelay 10000

  mapM_ (printFunc) $ embed
    (mips [i1, i2, i3, i4, i5, i6, i7, i8, foreverWait]) -- 使いたいSF
    (O, [(0.1, Just e) | e <- clocks])

main :: IO ()
main = mipsTest3
