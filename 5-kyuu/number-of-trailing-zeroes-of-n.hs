module Zeros where

zeros :: Int -> Int
zeros = recur 0
  where recur acc 0 = acc
        recur acc n = let r = n `div` 5 in recur (acc+r) r
