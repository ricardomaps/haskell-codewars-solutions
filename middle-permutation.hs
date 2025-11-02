module MiddlePermutation.JorgeVS.Kata where
import Data.List (sort, splitAt)
middlePermutation :: String -> String
middlePermutation  = go . sort
  where
  go s
    | null s = []
    | even (length s) = l : reverse (b ++ a)
    | otherwise = l : go (b ++ a)
    where
    middlePoint = pred . ceiling $ fromIntegral (length s) / 2
    (b, l:a) = splitAt middlePoint s
