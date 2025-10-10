module Codewars.Kata.Scrabble where
import Data.Maybe (fromJust, fromMaybe)
import Data.List

getBestWord :: [Int] -> [String] -> Int
getBestWord ps ws = 
  let
  points = zip ['A'..'Z'] ps
  getScore = sum . map (fromMaybe 0. (flip lookup points))
  scores = map getScore ws
  best = maximum scores
  in fromJust . findIndex (== best) $ scores
