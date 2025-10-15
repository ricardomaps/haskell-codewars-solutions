module RankVector where
import Data.Ord (Down, comparing)
import Data.List (groupBy, sort)
import Data.Maybe (fromJust)
import Data.Function (on)

ranks :: (Eq a, Ord a) => [a] -> [Int]
ranks l = fromJust . flip lookup rankMap <$> l
  where
  rankMap = map head . groupBy ((==) `on` fst) . flip zip [1..] . reverse . sort $ l

