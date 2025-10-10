module PellNumbers (pell) where

pell :: Int -> Integer
pell = head . (flip drop) pellSequence
  where pellSequence = 0 : 1 : zipWith (\x y -> 2 * x + y) (tail pellSequence) pellSequence
