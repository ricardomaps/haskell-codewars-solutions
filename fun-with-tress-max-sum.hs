module MaxSum (maxSum) where

import MaxSumPreload -- TreeNode = None | Node TreeNode Int TreeNode

maxSum :: TreeNode -> Int
maxSum None = 0
maxSum (Node None val None) = val
maxSum (Node None val right) = val + maxSum right
maxSum (Node left val None) = val + maxSum left
maxSum (Node left val right) = val + max (maxSum left) (maxSum right)
