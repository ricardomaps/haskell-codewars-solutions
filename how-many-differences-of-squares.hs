module DifferencesOfSquares.Kata (countSquareable) where

countSquareable :: Int -> Int
countSquareable n = n - (n `div` 2) + (n `div` 4)
