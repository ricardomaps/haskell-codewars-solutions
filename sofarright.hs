module SoFarRight where

import Prelude hiding (foldl, reverse)

foldl f z l = foldr (\x f' a -> f' (f a x)) id l z
