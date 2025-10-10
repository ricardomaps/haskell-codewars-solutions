module UniqueInOrder (uniqueInOrder) where

uniqueInOrder :: Eq a => [a] -> [a]
uniqueInOrder (x1:xs@(x2:_))
  | x1 == x2 = uniqueInOrder xs
  | otherwise = x1 : uniqueInOrder xs
uniqueInOrder xs = xs
