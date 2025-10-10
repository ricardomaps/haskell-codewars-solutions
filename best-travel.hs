module Codewars.G964.SumOfK where
import Data.List

maybeMaximum :: [Int] -> Maybe Int
maybeMaximum [] = Nothing
maybeMaximum xs = Just (maximum xs)

combinations :: Int -> [Int] -> [[Int]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

chooseBestSum :: Int -> Int -> [Int] -> Maybe Int
chooseBestSum t k = maybeMaximum . filter (<= t) . map sum . combinations k

