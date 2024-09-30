module InverseSlicer where

inverseSlice :: [a] -> Int -> Int -> [a]
inverseSlice lst a b = l ++ r
  where l = take a lst
        r = drop b lst
