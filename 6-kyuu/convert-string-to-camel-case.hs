module CamelCase (toCamelCase) where
import Data.Char (toUpper)

capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

chop :: Char -> String -> [String]
chop _ [] = []
chop c xss@(x:xs)
  | x == c = chop c xs
  | otherwise = let (f, r) = break (== c) xss
                in f : chop c r
  
toCamelCase :: String -> String
toCamelCase str  
  | null str = []
  | otherwise = let (firstWord:restWords) = if '_' `elem` str then chop '_' str else chop '-' str
                in firstWord ++ (concat $ map capitalize restWords)
