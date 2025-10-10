{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, KindSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Counting where
import Data.List (groupBy)
import Data.Function (on)
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Void
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Compose

import Data.Kind (Type)
import Control.Arrow ((***))

data Nat = Z | S Nat deriving (Show, Eq, Ord)

nat :: a -> (Nat -> a) -> Nat -> a
nat a _ Z = a
nat _ aa (S n) = aa n

iterNat :: (a -> a) -> Nat -> (a -> a)
iterNat _ Z = id
iterNat aa (S n) = aa . iterNat aa n

natUnderflow :: String
natUnderflow = "Nat is non-negative"

instance Num Nat where
  (+) = iterNat S
  a * b = iterNat (b+) a Z
  a - Z = a
  Z - _ = error natUnderflow
  S a - S b = a - b
  abs = id
  signum Z = Z
  signum (S _) = S Z
  fromInteger x
    | x < 0 = error natUnderflow
    | x == 0 = Z
    | otherwise = S $ fromInteger $ x - 1

instance Enum Nat where
  toEnum x
    | x < 0 = error natUnderflow
    | x == 0 = Z
    | otherwise = S $ toEnum $ x - 1
  fromEnum x = iterNat (+1) x 0

instance Real Nat where toRational = toRational . fromEnum

instance Integral Nat where
  quotRem _ Z = error "divide by zero"
  quotRem a b = until ((< b) . snd) (S *** subtract b) (Z, a)
  divMod = quotRem
  toInteger n = iterNat (+1) n 0

{- in Preloaded:
data Nat = Z | S Nat deriving (Show, Eq, Ord)
instance Num Nat -- so (2 :: Nat) == S (S Z)
instance Enum Nat -- so ([0..3] :: [Nat]) == [Z, S Z, S (S Z)]
instance Real Nat
instance Integral Nat -- so (2 ^ 3 :: Nat) == S (S (S (S (S (S (S (S Z)))))))
-}

newtype Count x = Count { getCount :: Nat } deriving (Show, Eq, Ord)

infinity :: Nat
infinity = S infinity

-- | helper functions
mapC :: (Nat -> Nat) -> Count a -> Count b
mapC f (Count n) = Count (f n)

liftC2 :: (Nat -> Nat -> Nat) -> Count a -> Count b -> Count c
liftC2 f (Count a) (Count b) = Count (f a b)

coerceC :: Count a -> Count b
coerceC = Count . getCount

-- | Countable
class Countable (c :: Type) where
  count :: Count c
  -- if you are using `Proxy` implement `count` from `count'` and vice versa
  -- count' :: Proxy c -> Count c
  -- count' = error "from count"

instance Countable Void where count = Count 0
instance Countable () where count   = Count 1
instance Countable Bool where count = Count 2
instance Countable Nat where count  = Count infinity

-- | Factor
class Factor (f :: Type -> Type) where
  factor :: Count c -> Count (f c)
  -- factor' :: Proxy f -> Count c -> Count (f c) -- optional

instance (Factor f, Countable c) => Countable (f c) where
  count = factor (count @c)

instance Factor Maybe where factor = Count . succ . getCount
instance Factor Identity where factor = coerceC
instance Factor Proxy where factor = const (Count 1)
instance Factor Count where factor = const (Count infinity)
instance Factor [] where factor (Count n) = case n of Z -> Count 1; _ -> Count infinity
instance Countable c => Factor (Const c) where factor = const (coerceC $ count @c)
instance Countable c => Factor (Either c) where factor (Count n) = Count $ getCount (count @c) + n
instance Countable c => Factor ((,) c) where factor (Count n) = Count (n * (getCount (count @c)))
instance Countable c => Factor ((->) c) where factor (Count n) = Count $ n ^ getCount (count @c)
instance (Factor f, Factor g) => Factor (Sum f g) where factor c = Count $ getCount (factor @f c) + getCount (factor @g c)
instance (Factor f, Factor g) => Factor (Product f g) where factor c = Count $ getCount (factor @f c) * getCount (factor @g c)
instance (Factor f, Factor g) => Factor (Compose f g) where factor = coerceC . factor @f . factor @g

-- | Listable
class Countable a => Listable (a :: Type) where
  list :: [a]
  -- list' :: Proxy a -> [a] -- optional
-- Data.List.genericLength (list :: [a]) `shouldBe` getCount (count :: Count a)

instance Listable Void where list = []
instance Listable () where list = [()]
instance Listable Bool where list = [True, False]
instance Listable Nat where list = iterate S Z

instance Listable c => Listable (Maybe c) where list = Nothing : (Just <$> list @c)
instance Listable c => Listable [c] where list = concat $ iterate (\l -> (:) <$> (list @c) <*> l) [[]]
instance (Listable a, Listable b) => Listable (Either a b) where list = (Left <$> (list @a)) ++ (Right <$> (list @b))
instance (Listable a, Listable b) => Listable (a, b) where list = (,) <$> (list @a) <*> (list @b)
instance (Eq a, Listable a, Listable b) => Listable (a -> b) where
  list =
    if impossible then []
    else
      map (\m -> fromJust . flip lookup m)
      . foldr (\l a -> (:) <$> l <*> a ) [[]]
      . groupBy ((==) `on` fst)
      $ list @(a, b)
    where impossible = (getCount (count @b) == 0) && (getCount (count @a) > 0)
