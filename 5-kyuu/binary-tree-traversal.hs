module BinaryTreeTraversal
  ( preOrder
  , inOrder
  , postOrder
  ) where

import BinaryTreeTraversal.Types

{-
data Tree a = Nil | Node (Tree a) a (Tree a)
-}

-- 1.) Root node, 2.) traverse left subtree, 3.) traverse right subtree.
preOrder :: Tree a -> [a]
preOrder = go [] 
  where go acc (Node left val right) = val : go (go acc right) left
        go acc Nil = acc

-- 1.) Traverse left subtree, 2.) root node, 3.) traverse right subtree.
inOrder :: Tree a -> [a]
inOrder = go []
  where go acc (Node left val right) = go (val : go acc right) left
        go acc Nil = acc
        
-- 1.) Traverse left subtree, 2.) traverse right subtree, 3.) root node.
postOrder :: Tree a -> [a]
postOrder = go []
  where go acc (Node left val right) = go (go (val:acc) right) left
        go acc Nil = acc
