module BFTSolution where

import Data.Traversable (foldMapDefault)
data Tree a = a :& [Tree a] deriving (Eq, Ord, Show, Functor)

instance Foldable Tree where
  foldMap = foldMapDefault

instance Traversable Tree where
  traverse f (x :& _) = go [[x]]
    where go l = map (traverse f) l `zipWith `

