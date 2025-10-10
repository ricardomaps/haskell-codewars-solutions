module SuffixSums (suffixSums) where

suffixSums :: [Int] -> [Int]
suffixSums = scanr1 (+)
