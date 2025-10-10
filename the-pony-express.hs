module PonyExpress where

riders :: [Int] -> Int
riders = snd . foldl (\(t, r) m -> if m > t then (100-m, r+1) else (t-m, r)) (0, 0)
