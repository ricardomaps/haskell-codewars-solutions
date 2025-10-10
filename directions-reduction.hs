module Codewars.Kata.Reduction where
import Codewars.Kata.Reduction.Direction

-- data Direction = North | East | West | South deriving (Eq)

dirReduce :: [Direction] -> [Direction]
dirReduce [] = []
dirReduce [x] = [x]
dirReduce (x:xs) 
  | null reducedNext = [x]
  | otherwise =
    case (x, head reducedNext) of
      (North, South) -> tail reducedNext 
      (South, North) -> tail reducedNext
      (West, East) -> tail reducedNext
      (East, West) -> tail reducedNext
      _ -> x : reducedNext
  where reducedNext = dirReduce xs
