-- 配線で31-0の31-25とかを取得するときの範囲を試す関数の試し

range :: Int -> Int -> [Int] -> [Int]
range s e xs = take (s-e+1) . drop (n-1-s) $ xs
  where n = length xs

main :: IO ()
main = do
  print $ range 3 1 [5,4..0] -- 5-0の3-1をとる
  -- [3,2,1]
