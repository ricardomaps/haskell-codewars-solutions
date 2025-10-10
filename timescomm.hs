{-# LANGUAGE TypeOperators, TypeFamilies, GADTs, UndecidableInstances #-}

module Kata.TimesComm where

-- import Kata.TimesComm.Definitions

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
  NumZ :: Natural Z
  NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
  EqlZ :: Equal Z Z
  EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)

-- | Peano definition of multiplication.
type family (:*:) (n :: *) (m :: *) :: *
type instance Z :*: m = Z
type instance S n :*: m = m :+: (n :*: m)

-- This will be helpful

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


transitive' :: Equal a b -> Equal a c -> Equal c b
transitive' EqlZ     EqlZ     = EqlZ
transitive' (EqlS n) (EqlS m) = EqlS (transitive' n m)

-- reify :: Equal n m -> Natural n
-- reify EqlZ     = NumZ
-- reify (EqlS n) = NumS (reify n)

plus :: Natural n -> Natural m -> Natural (n :+: m)
plus NumZ m     = m
plus (NumS n) m = NumS (plus n m)

times :: Natural n -> Natural m -> Natural (n :*: m)
times NumZ     m = NumZ
times (NumS n) m = plus m (times n m)

plusZero :: Natural n -> Equal n (n :+: Z)
plusZero NumZ     = EqlZ
plusZero (NumS n) = EqlS (plusZero n)

plusSucc :: Natural n -> Natural m -> Equal (n :+: S m) (S (n :+: m))
plusSucc NumZ     m = reflexive (NumS m)
plusSucc (NumS n) m = EqlS (plusSucc n m)

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-plus-c-equals-a-plus-b-plus-c-prove-it/haskell
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ     m p = reflexive (m `plus` p)
plusAssoc (NumS n) m p = EqlS (plusAssoc n m p)

-- | You need this! Copy your solution from
-- https://www.codewars.com/kata/a-plus-b-equals-b-plus-a-prove-it/haskell
plusComm :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusComm NumZ     m = plusZero m
plusComm (NumS n) m = transitive (EqlS (plusComm n m)) (symmetric (plusSucc m n))

-- This will also be helpful
zeroComm :: Natural a -> Equal Z (a :*: Z)
zeroComm NumZ     = EqlZ
zeroComm (NumS n) = zeroComm n

plus' :: Equal a b -> Equal c d -> Equal (a :+: c) (b :+: d)
plus' EqlZ       eq = eq
plus' (EqlS eq') eq = EqlS (plus' eq' eq)

timesSucc :: Natural n -> Natural m -> Equal (n :*: S m) (n :+: (n :*: m))
timesSucc NumZ     m = EqlZ
timesSucc (NumS n) m =
  EqlS
  $ flip transitive (symmetric $ plusAssoc n m (n `times` m))
  $ flip transitive (plus' (plusComm m n) (reflexive $ n `times` m))
  $ flip transitive (plusAssoc m  n (n `times` m))
  $ plus' (reflexive m)
  $ timesSucc n m

-- This is the proof that the kata requires.
-- | a * b = b * a
timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm NumZ     m = zeroComm m
timesComm (NumS n) m =
  flip transitive (symmetric $ timesSucc m n)
  $ plus' (reflexive m)
  $ timesComm n m

