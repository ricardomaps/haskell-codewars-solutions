module Clicker (clickerSolver) where

clickerSolver :: Int -> Int -> Int
clickerSolver up goal
  | goal <= 0  = 0
  | up <= 0    = -1
  | otherwise  = go goal up
  where 
  go goal cpc
    | cpc ^ 3 >= goal = notTake
    | otherwise = min (go goal (cpc + up) + cpc ^ 2 + 1) notTake
    where notTake = ceiling $ fromIntegral goal / fromIntegral cpc

-- 5 + 17 + 18 
