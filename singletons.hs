{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool where
  m        :< Zero     = False
  Zero     :< Succ n   = True
  (Succ m) :< (Succ n) = m :< n

type family Add (a :: Nat) (b :: Nat) :: Nat where
  Add Zero     m = m
  Add (Succ n) m = Succ (Add n m)

type family Sub (a :: Nat) (b :: Nat) :: Nat where
  Sub Zero _ = Zero
  Sub n Zero = n
  Sub (Succ n) (Succ m) = Sub n m


type family Min (a :: Nat) (b :: Nat) :: Nat where
  Min Zero _ = Zero
  Min _ Zero = Zero
  Min (Succ n) (Succ m) = Succ (Min n m)

map :: (a -> b) -> Vec a n -> Vec b n
map _ VNil         = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons x _) = x
index (SSucc n) (VCons _ xs) = index n xs

replicate :: s -> SNat a -> Vec s a
replicate _ SZero     = VNil
replicate x (SSucc n) = VCons x (replicate x n)

-- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a n -> Vec b n -> Vec c n
zipWith _ VNil         VNil         = VNil
zipWith f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWith f xs ys)

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil         ++ b = b
(VCons x xs) ++ b = VCons x (xs ++ b)

-- The semantics should match that of take for normal lists.
take :: SNat n -> Vec a m -> Vec a (Min n m)
take SZero     _            = VNil
take _         VNil         = VNil
take (SSucc n) (VCons x xs) = VCons x (take n xs)

-- The semantics should match that of drop for normal lists.
drop :: SNat a -> Vec s n -> Vec s (Sub n a)
drop SZero     v            = v
drop _         VNil         = VNil
drop (SSucc n) (VCons _ xs) = drop n xs

head ::(Zero :< n ~ True) => Vec s n -> s
head (VCons x _) = x

tail :: (Zero :< n ~ True) => Vec s n -> Vec s (Sub n (Succ Zero))
tail (VCons _ xs) = xs
