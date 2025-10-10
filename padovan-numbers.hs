module Padovan where

padovanSeq = 1 : 1 : 1 : zipWith (+) padovanSeq (tail padovanSeq)

padovan :: Int -> Integer
padovan n = padovanSeq !! n
