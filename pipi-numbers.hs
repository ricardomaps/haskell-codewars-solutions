module Pipi (pipi) where

pipiSeq :: [Integer]
pipiSeq = 0 : go 1
  where go i = foldl (\x n -> (x-n)^2) (fromIntegral i) (take i pipiSeq) : go (i+1)
  
pipi :: Int -> Integer
pipi n = pipiSeq !! n
