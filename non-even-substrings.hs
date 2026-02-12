module NonEvenSubstrings where 
import Data.Char (digitToInt)

solve :: String -> Int 
solve = sum . map (\(i, _) -> i+1) . filter (odd . snd) . zip [0..] . map digitToInt
