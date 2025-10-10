module Codewars.Kata.TenMinuteWalk where
import Data.List (foldl')

isValidWalk :: [Char] -> Bool
isValidWalk walk = rightLength 0 walk && circlesBack walk
  where rightLength a [] = a == 10
        rightLength a w | a > 10 = False | otherwise = rightLength (a+1) (tail w)
        circlesBack w = (0, 0) == foldl' move (0, 0) w
        move (x, y) d = case d of
                          'n' -> (x, y+1)
                          's' -> (x, y-1)
                          'w' -> (x-1, y)
                          'e' -> (x+1, y)
