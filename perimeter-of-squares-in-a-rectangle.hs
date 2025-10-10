module Codewars.Kata.Perimeter where
import Data.List

perimeter :: Integer -> Integer
perimeter n = (*4) . sum . take (fromInteger (succ n)) $ fib where fib = 1 : 1 : zipWith (+) fib (tail fib)
