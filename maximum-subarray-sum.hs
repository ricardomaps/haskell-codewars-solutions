module MaxSequence where

-- Return the greatest subarray sum within the array of integers passed in.
maxSequence :: [Int] -> Int
maxSequence = maximum . scanl (\b a -> max (b+a) a) 0
