{-# LANGUAGE FunctionalDependencies #-}
module PolyvariadicFunctions where

class PolyAdd a where
  addPoly :: Int -> a

instance PolyAdd Int where
  addPoly !acc = acc

instance (b ~ Int, PolyAdd a) => PolyAdd (b -> a) where
  addPoly !acc x = addPoly (acc + x)

-- `polyAdd` sums its arguments, all `Int`s.
polyAdd :: PolyAdd a => a
polyAdd = addPoly 0

class PolyList b a | a -> b where
  listPoly :: [b] -> a

instance PolyList b [b] where
  listPoly = reverse

instance PolyList b a => PolyList b (b -> a) where
  listPoly acc x = listPoly (x:acc)

-- `polyList` turns its arguments into a list, polymorphically.
polyList :: forall a b. PolyList b a => a
polyList = listPoly []

class PolyWords a where
  wordsPoly :: String -> a

instance PolyWords String where
  wordsPoly acc = acc

instance PolyWords a => PolyWords (String -> a) where
  wordsPoly "" s  = wordsPoly s
  wordsPoly acc s = wordsPoly (" " <> acc <> s)

-- `polyWords` turns its arguments into a spaced string.
polyWords :: PolyWords a => a
polyWords = wordsPoly ""
