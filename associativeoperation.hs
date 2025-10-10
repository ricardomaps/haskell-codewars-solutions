module AssociativeOperation (computeRanges) where

computeRanges :: [t] -> (t -> t -> t) -> [(Int,Int)] -> [t]
computeRanges arr op ranges = 
