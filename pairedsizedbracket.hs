{-# LANGUAGE KindSignatures, DataKinds, GADTs, TypeFamilies #-}

module PairedSizedBracket where

import Data.List (nub, sort)
import Data.Kind (Type)

data Nat = Z | S Nat deriving Show

instance Show (Paren a n) where
    show PEmpty = ""
    show (PLeft p) = '(' : show p
    show (PRight p) = ')' : show p

instance Eq (Paren a n) where
    PEmpty == PEmpty = True
    (PLeft n1) == (PLeft n2)  = n1 == n2
    (PRight n1) == (PRight n2)  = n1 == n2
    _ == _ = False

instance Ord (Paren a n) where
    a <= b = show a <= show b

-- additional nat definition is needed to encode different types to different numbers. 
data Nt :: Nat -> Type where
    Zt :: Nt Z
    St :: Nt a -> Nt (S a)

type family Add (a :: Nat) (b :: Nat) :: Nat where
  Add Z     m = m
  Add m     Z = m
  Add (S m) n = S (Add m n)

-- data definition, implement Paren encoding to contain balancing and size information.
-- expression p is a paired brackets if and only if p is with type Paren a Z, 
-- where a contains the size information
data Paren :: Nat -> Nat -> Type where
    PEmpty :: Paren Z Z
    PLeft  :: Paren m (S n) -> Paren m n
    PRight :: Paren m n -> Paren (S m) (S n)

-- use the Paren data definition
-- makeNestedParenOfSize :: Int -> Paren 
-- makeNestedParenOfSize n | n <= 0 = PEmpty
-- makeNestedParenOfSize n = foldr ($) PEmpty (replicate n PLeft ++ replicate n PRight)
-- implement the above function with your new Paren definition
makeNestedParenOfSize :: Nt a -> Paren a Z
makeNestedParenOfSize = insertLeft <*> insertRight 
  where
    insertRight :: Nt a -> Paren a a
    insertRight Zt     = PEmpty
    insertRight (St n) = PRight (insertRight n)

    insertLeft :: Nt a -> Paren m a -> Paren m Z
    insertLeft Zt     p = p
    insertLeft (St n) p = insertLeft n (PLeft p)

-- now try something more interesting, implement a function
-- to generate all possible balanced parens of specific size
-- (order does not matter, but should not contain duplication)
-- >>> map show $ makeParensOfSize Zt 
-- [""]
-- >>> map show $ makeParensOfSize (St Zt)
-- ["()"]
-- >>> map show $ makeParensOfSize (St $ St $ Zt)
-- ["()()","(())"]
-- >>> map show $ makeParensOfSize (St $ St $ St $ Zt)
-- ["()()()","(())()","(()())","()(())","((()))"]
makeParensOfSize :: Nt a -> [Paren a Z]
makeParensOfSize Zt = [PEmpty]
makeParensOfSize _ = undefined
