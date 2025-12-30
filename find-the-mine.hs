module MineLocation where

import Data.Array
import Data.Tuple (swap)

mineLocation :: Array (Int, Int) Int -> Maybe (Int, Int)
mineLocation = lookup 1 . map swap . assocs
