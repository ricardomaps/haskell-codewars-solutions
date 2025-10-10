module Kata.TargetGame (targetGame) where

targetGame :: [Int] -> Int
targetGame = snd . foldr (\x (p1, p2) -> (p2, max (x + p1) p2)) (0, 0)
