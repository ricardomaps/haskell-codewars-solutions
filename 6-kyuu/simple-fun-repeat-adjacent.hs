module RepeatAdjacent (repeatAdjacent) where
import Data.List

repeatAdjacent :: String -> Int
repeatAdjacent =  length . filter (\x -> length x > 1) . groupBy (\a b -> a > 1 && b > 1) . map length . group
