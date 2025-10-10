module Kata.SmallestPossibleSum where

smallestPossibleSum :: (Integral a) => [a] -> a
smallestPossibleSum = (*) <$> fromIntegral . length <*> foldl1 gcd
