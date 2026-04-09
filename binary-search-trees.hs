module BinarySearchTrees () where

-- import Preloaded (Tree(Tree))
data Tree = Tree Node (Maybe Tree) (Maybe Tree) deriving ()
newtype Node = Node Char deriving (Eq,Ord, Show)

instance Show Tree where
  show (Tree n Nothing Nothing) = "[" ++ show n ++ "]"
  show (Tree n l r)             = "[" ++ showMaybe l ++ " " ++ show n ++ " " ++ showMaybe r ++  "]"
    where
    showMaybe Nothing  = "_"
    showMaybe (Just t) = show t

instance Eq Tree where
  (Tree n1 l1 r1) == (Tree n2 l2 r2) = n1 == n2 && l1 == l2 && r1 == r2

