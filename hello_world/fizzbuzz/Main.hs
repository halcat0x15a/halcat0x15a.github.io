import Data.Foldable (for_)

main :: IO ()
main = for_ [1..100] $ putStrLn . fizzbuzz
  where
    fizzbuzz n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod`  3 == 0 = "Fizz"
      | n `mod`  5 == 0 = "Buzz"
      | otherwise       = show n
