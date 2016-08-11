import           Control.Monad
import           System.Environment
import           Text.Printf

main :: IO ()
main = do
  args <- map read <$> getArgs :: IO [Int]
  case args of
    [n] -> do
      forM_ [1..n] $ \i -> do
        printf "type N%d = Succ N%d\n" i (i-1)

      forM_ [1..n] $ \i -> do
        printf "n%d = SSucc n%d\n" i (i-1)

      forM_ [1..n] $ \i -> printf "n%d," i
      putStrLn ""

      forM_ [1..n] $ \i -> printf "N%d," i
      putStrLn ""

    _ -> putStrLn ("input error\nusage: auto-creator 32")
