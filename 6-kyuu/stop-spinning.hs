module Codewars.Kata.Spinning where

spinWords :: String -> String
spinWords = unwords . map (\w -> if length w >=5 then reverse w else w) . words
