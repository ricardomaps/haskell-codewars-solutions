module SumGroups.Kata (sumGroups) where
import Data.List (groupBy)
import Data.Function (on)

sumGroups :: [Int] -> Int
sumGroups nums
  | all (\ns -> length ns == 1) numsByParity = length numsByParity
  | otherwise = sumGroups (sum <$> numsByParity)
  where numsByParity = groupBy ((==) `on` even) nums
