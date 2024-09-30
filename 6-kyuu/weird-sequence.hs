module WeirdSequence (weird) where

weird :: Int -> Int
weird i | even i = 1 + i `div` 2
        | otherwise = weird $ i `div` 2
