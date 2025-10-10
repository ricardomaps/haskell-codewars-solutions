module Codewars.SuperCoordinateSum where

superSum :: Integer -> Integer -> Integer
superSum d n = d * n^d * (n-1) `div` 2
