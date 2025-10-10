module HughesList where

newtype Hughes a = Hughes ([a] -> [a])

runHughes :: Hughes a -> [a]
runHughes (Hughes k) = k []

mkHughes :: [a] -> Hughes a
mkHughes l = Hughes ((++) l)

------------------------------------------------------------

consDumb :: a -> Hughes a -> Hughes a
consDumb a h = mkHughes (a : runHughes h)

cons :: a -> Hughes a -> Hughes a
cons x (Hughes f) = Hughes ((x:) . f)

------------------------------------------------------------

appendDumb :: Hughes a -> Hughes a -> Hughes a
appendDumb a b = mkHughes (runHughes a ++ runHughes b)

instance Semigroup (Hughes a) where
  (Hughes f) <> (Hughes g) = Hughes (f . g)
  
instance Monoid (Hughes a) where
  mempty = Hughes id
  
------------------------------------------------------------

snocDumb :: Hughes a -> a -> Hughes a
snocDumb l a = mkHughes (runHughes l ++ [a])

snoc :: Hughes a -> a -> Hughes a
snoc (Hughes f) x = Hughes (f . (x:))
