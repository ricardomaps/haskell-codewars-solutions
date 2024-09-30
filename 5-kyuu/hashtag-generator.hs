module Codewars.Kata.Hashtag where
import Data.Char (toUpper, isSpace)

isBlank :: String -> Bool
isBlank = null . dropWhile isSpace

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

generateHashtag :: String -> Maybe String
generateHashtag str
  | isBlank str || length str > 140 = Nothing
  | otherwise = Just . ('#':) . concatMap capitalize . words $ str
