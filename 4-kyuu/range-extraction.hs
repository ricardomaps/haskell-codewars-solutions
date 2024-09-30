module RangeExtractor.JorgeVS.Kata where
import Data.List
import Data.Function (on)

groupRanges :: [Integer] -> [[Integer]]
groupRanges = 
          map (map snd)
         . groupBy ((==) `on` fst) 
         . tail
         . scanl (\(g, prev) x -> if x-prev == 1 then (g, x) else (g+1, x)) (0, 0)
         
showRange :: [Integer] -> String
showRange [] = ""
showRange [a] = show a
showRange [a, b] = show a ++ "," ++ show b
showRange r = show (head r) ++ "-" ++ show (last r)
         
solution :: [Integer] -> String
solution = intercalate "," . map showRange . groupRanges
