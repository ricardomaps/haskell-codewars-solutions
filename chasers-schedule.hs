module Chaser (speedAndTime) where

speedAndTime :: Int -> Int -> Int
speedAndTime s t = s * t + sum (take space [s,s-3..0])
  where space = (t+1) `div` 2
