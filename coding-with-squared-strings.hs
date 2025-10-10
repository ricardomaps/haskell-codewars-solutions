module Codewars.G964.CoddecSqStrings where
import Data.List
import Data.List.Split (chunksOf)

clockwiseRotation :: [String] -> [String]
clockwiseRotation = map reverse . transpose

counterClockwiseRotation :: [String] -> [String]
counterClockwiseRotation = reverse . transpose

pad :: Int -> String -> String
pad p s = s ++ replicate p '\v' 

code :: [Char] -> [Char]
code s = intercalate "\n" . clockwiseRotation . chunksOf n . pad p $ s
  where l = length s 
        n = ceiling (sqrt . fromIntegral $ l)
        p = n^2 - l

decode :: [Char] -> [Char]
decode = dropWhileEnd (== '\v') . concat . counterClockwiseRotation . lines
