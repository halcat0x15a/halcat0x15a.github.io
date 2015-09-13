import Control.Monad (forM_)

fib :: [Int]
fib = 0 : 1 : zipWith (+) fib (tail fib)

main :: IO ()
main = forM_ (take 50 fib) (putStrLn . show)
