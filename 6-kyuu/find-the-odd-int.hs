module Codewars.Kata.FindOdd where
import Data.Bits (xor)

findOdd :: [Int] -> Int
findOdd = foldl xor 0 
