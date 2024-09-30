module SequenceA112382 where

sdfs = 1 : 1 : genSdfs 2 (tail sdfs)
genSdfs n (x:xs) = [n..n+x-1] ++ x : genSdfs (n+x) xs

a112382 :: Int -> Int
a112382 n = sdfs !! n

