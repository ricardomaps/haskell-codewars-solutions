module Fold (foldWhile) where

foldWhile :: (b -> Bool) -> (b -> a -> b) -> b -> [a] -> b
foldWhile _ _ z [] = z
foldWhile p f z (x:xs)
  | p z' = foldWhile p f z' xs
  | otherwise = z
  where z' = f z x
