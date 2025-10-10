module Spiral where
import Data.Function (on)
import Data.List (groupBy)
import Data.Array

spiralize :: Int -> [[Int]]
spiralize n = toListOfLists $ spiralBoard // ((, 1) <$> stars 1 n)  -- Make a snake
  where
    spiralBoard = listArray ((1, 1), (n, n)) (repeat 0)
    stars l h
      | l > h = []
      | h-l == 1 = [(l, l), (l, h), (h, h)]
      | otherwise = 
          [(l, y) | y <- [l..h]]     ++
          [(x, h) | x <- [l+1..h]]   ++
          [(h, y) | y <- [l..h-1]]   ++
          [(x, l) | x <- [l+2..h-1]] ++
          [(l+2, l+1) | l+2 <= h-2]  ++
          stars (l+2) (h-2)
    toListOfLists arr = map (map snd) . groupBy ((==) `on` (fst . fst)) $ assocs arr
