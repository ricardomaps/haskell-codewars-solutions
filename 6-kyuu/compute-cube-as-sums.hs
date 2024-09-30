module CubeAsSums (findSummands) where 

findSummands :: Int -> [Int]
findSummands n = take n (iterate (+2) a)
  where a = n^2 - n + 1
