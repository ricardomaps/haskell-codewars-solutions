module Codewars.G964.Johnann where

annSeq :: [Integer]
annSeq = 1 : genAnn (tail johnSeq) 1 1
  where genAnn j a i = let n = i-(head j)
                           nj = if n > a then tail j else j 
                           in n : genAnn nj n (i+1)

johnSeq :: [Integer]
johnSeq = 0 : genJohn annSeq 0 1
  where genJohn a j i = let n = i-(head a)
                            na = if n > j then tail a else a
                            in n : genJohn na n (i+1)

ann :: Int -> [Integer]
ann n = take n annSeq
                           
john :: Int -> [Integer]
john n = take n johnSeq

sumAnn :: Int -> Integer
sumAnn = sum . ann
sumJohn :: Int -> Integer
sumJohn = sum . john
