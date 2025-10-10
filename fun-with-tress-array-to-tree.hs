module ArrayToTree (toTree) where

import Preloaded (TreeNode(..)) -- TreeNode = None | Node TreeNode Int TreeNode
import Data.Vector.Unboxed (Vector, (!?))

toTree :: Vector Int -> TreeNode
toTree v = go 0
  where go i = 
          case v !? i of
            Nothing -> None
            Just val -> Node (go (2*i + 1)) val (go (2*i + 2))
