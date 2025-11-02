module UnionOfIntervals.Kata (intervalInsert) where

type Interval = (Int,Int)

intervalInsert :: [Interval] -> Interval -> [Interval]
intervalInsert xs interval = foldr merge [interval] xs 
  where
  merge i2@(l2, h2) (i1@(l1, h1):xs)
    | l2 > h1 = i1:i2:xs
    | l1 > h2 = i2:i1:xs
    | otherwise = (min l1 l2, max h1 h2):xs
