module Feynman where

countSquares :: Integer -> Integer
countSquares n | n == 0 = 0
               | otherwise = n * n + countSquares (n - 1)
