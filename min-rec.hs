

-- minを再帰で書く試み

myMin :: Int -> Int -> Int
myMin 0 m = 0
myMin n 0 = 0
myMin n m = myMin (n-1) (m-1) + 1

main :: IO ()
main = do
  print $ myMin 4 5
  print $ myMin 5 4
  print $ myMin 5 5
  print $ myMin 1 8
