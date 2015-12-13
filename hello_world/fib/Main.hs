import Data.Foldable (for_)

main :: IO ()
main = for_ (take 50 fib) (putStrLn . show)
  where
    fib = 0 : 1 : zipWith (+) fib (tail fib)
