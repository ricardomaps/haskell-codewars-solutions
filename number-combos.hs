module Codewars.Kata.Combos where

combos :: Int -> [[Int]]
combos n = go 1 n
  where
  go _ 0 = [[]]
  go m n = do
    i <- [m..n]
    s <- go i (n-i)
    return (i : s)
