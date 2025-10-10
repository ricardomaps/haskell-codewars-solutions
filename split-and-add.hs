module SplitAndAdd (splitAndAdd) where

splitAndAdd :: [Int] -> Int -> [Int]
splitAndAdd lst n
  | n == 0 || length lst == 1 = lst
  | otherwise = let (fstLst, sndLst) = splitAt (length lst `div` 2) lst
                    fstLst' = if length fstLst < length sndLst then 0:fstLst else fstLst 
                    newLst = zipWith (+) fstLst' sndLst
                in splitAndAdd newLst (pred n)
