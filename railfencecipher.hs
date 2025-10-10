module RailFenceCipher.Kata (encode,decode) where
import Data.List (sortOn)

rails :: Int -> [Int]
rails n = cycle $ [1..n] ++ [n-1,n-2..2]
  
encode :: [a] -> Int -> [a]
encode s n = map snd . sortOn fst . zip (rails n) $ s

decode :: [a] -> Int -> [a]
decode s n = map fst . sortOn (snd . snd) . zip s . sortOn fst . take (length s) . zip (rails n) $ [1..]
