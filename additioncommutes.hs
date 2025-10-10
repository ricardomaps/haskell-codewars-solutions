{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Kata.AdditionCommutes
  ( plusCommutes ) where

import Data.Kind (Type)

-- import Kata.AdditionCommutes.Definitions
--   ( Z, S
--   , Natural(..), Equal(..)
--   , (:+:))

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ     = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ     = EqlZ
symmetric (EqlS n) = EqlS (symmetric n)

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ     EqlZ     = EqlZ
transitive (EqlS n) (EqlS m) = EqlS (transitive n m)

plusZero :: Natural n -> Equal n (n :+: Z)
plusZero NumZ     = EqlZ
plusZero (NumS n) = EqlS (plusZero n)

plusSucc :: Natural n -> Natural m -> Equal (n :+: S m) (S (n :+: m))
plusSucc NumZ     m = reflexive (NumS m)
plusSucc (NumS n) m = EqlS (plusSucc n m)

-- This is the proof that the kata requires.
-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ     m = plusZero m
plusCommutes (NumS n) m = transitive (EqlS (plusCommutes n m)) (symmetric (plusSucc m n))

-- For reference, here are the definitions, if you
-- want to copy them into an IDE:


-- For older GHC where Type is not in Prelude

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: Type -> Type where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: Type -> Type -> Type where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: Type) (m :: Type) :: Type
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)
