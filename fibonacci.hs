module Fibonacci (fib) where
import Data.Vector

fibSequence :: Vector Integer
fibSequence = generate 2000000 (\i -> if i < 2 then fromIntegral i else fibSequence ! (i-1) + fibSequence ! (i-2))

fib :: Integer -> Integer
fib n
  | n < 0 = fibSequence ! (fromIntegral $ -n) * if odd n then 1 else -1
  | otherwise = fibSequence ! (fromIntegral n)
