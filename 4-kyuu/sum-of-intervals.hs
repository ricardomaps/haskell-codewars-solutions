module SumOfIntervals (sumOfIntervals) where
import Data.Tuple (swap)
import Data.List (sort)

sumOfIntervals :: [(Int, Int)] -> Int
sumOfIntervals = foldl (\a i -> (+a) $ uncurry (-) $ swap i ) 0 . mergeIntervals . sort
  where 
    mergeIntervals :: [(Int, Int)] -> [(Int, Int)]
    mergeIntervals ((b1, e1):(b2, e2):xs)
          | e1 >= b2 = mergeIntervals $ (b1, max e1 e2) : xs
          | otherwise = (b1, e1) : mergeIntervals ((b2, e2):xs)
          
    mergeIntervals lst = lst
