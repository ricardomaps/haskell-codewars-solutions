module HowManyNumbers (findAll) where

-- findAll :: Int -> Int -> (Int,Maybe Int,Maybe Int)
findAll :: Int -> Int -> [Int]
findAll s d = go s d 0 0
  where
    go 0 0 n = n
    go s d n = concatMap (\x -> go (s-x) (d-1) ) [n `mod` 10..9]
