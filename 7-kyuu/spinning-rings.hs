module SpinningRings (spinningRings) where

spinningRings :: Int -> Int -> Int
spinningRings innerMax outerMax = aux innerMax 1 1
  where 
  aux a b m | a == b = m
            | otherwise = aux ((a-1) `mod` (innerMax+1)) ((b+1) `mod` (outerMax+1)) (m+1)
