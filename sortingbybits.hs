module SortingByBits (sortByBit) where

import Data.Word (Word32)
import Data.List (sortOn)
import Data.Bits (popCount)
import Control.Arrow ((&&&))

sortByBit :: [Word32] -> [Word32]
sortByBit = sortOn (popCount &&& id)
