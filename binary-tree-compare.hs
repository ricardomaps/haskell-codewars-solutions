module BinaryTreeCompare (BinaryTreeCompare.compare) where
import Prelude hiding (compare)
import Preloaded (Tree(..)) -- data Tree = Node { val :: Int, left, right :: Maybe Tree }

compare :: Maybe Tree -> Maybe Tree -> Bool
compare a b =
  case (a, b) of
    (Nothing, Nothing) -> True
    (Just a, Just b) -> val a == val b && compare (left a) (left b) && compare (right a) (right b)
    _ -> False
