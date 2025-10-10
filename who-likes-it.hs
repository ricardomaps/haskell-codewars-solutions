module Likes where

likes :: [String] -> String
likes xs
  | length xs == 0 = "no one likes this"
  | length xs == 1 = xs !! 0 ++ " likes this"
  | length xs == 2 = xs !! 0 ++ " and " ++ xs !! 1 ++ " like this"
  | length xs == 3 = xs !! 0 ++ ", " ++ xs !! 1 ++ " and " ++ xs !! 2 ++ " like this"
  | otherwise = xs !! 0 ++ ", " ++ xs !! 1 ++ " and " ++ show (length xs - 2) ++ " others like this"
