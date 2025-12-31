module Codewars.Suffixes where

numberToOrdinal :: Int -> String
numberToOrdinal 0 = "0"
numberToOrdinal number = show number <> suffixFor number
  where
  suffixFor n
    | n > 10 && n < 20 = "th"
    | n `mod` 10 == 1 = "st"
    | n `mod` 10 == 2 = "nd"
    | n `mod` 10 == 3 = "rd"
    | otherwise = "th"
