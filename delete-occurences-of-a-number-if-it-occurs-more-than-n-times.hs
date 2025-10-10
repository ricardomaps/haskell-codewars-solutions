module Codewars.Kata.Deletion where
import Data.Map as Map

deleteNth :: [Int] -> Int -> [Int]
deleteNth lst n = 
  recur (Map.empty) lst
  where
  recur _ [] = []
  recur m (x:xs)
    | Just v <- Map.lookup x m, v >= n = recur m xs
    | otherwise = x : recur (Map.insertWith (+) x 1 m) xs
