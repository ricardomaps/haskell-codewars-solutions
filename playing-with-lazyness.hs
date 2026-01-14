module Laziness where
import Data.List(find)
import Data.Maybe(fromJust)

type Matrix = [[Bool]]

findTrue :: Matrix -> (Int, Int)
findTrue matrix = fromJust $ find (\(x, y) -> matrix !! x !! y) [(i, j-i) | j <- [0..], i <- [0..j]]

