module Codewars.G964.Sumdigpow where

digits :: Int -> [Int]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

sumDigPow :: Int -> Int -> [Int]
sumDigPow a b = [n | n <- [a..b], isEureka n]
  where 
  isEureka n = (sum . zipWith (flip (^)) [1..] $ digits n) == n
  
