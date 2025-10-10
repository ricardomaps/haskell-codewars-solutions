module LargestProduct where
import Data.Char (digitToInt)
import Data.List.Split (chunksOf)

greatestProduct :: String -> Int
greatestProduct = maximum . map product . windowed . map digitToInt
  where windowed lst 
          | length lst < 5 = []
          | otherwise = (take 5 lst) : windowed (tail lst)
