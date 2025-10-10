module Haskell.Codewars.Church (pred,sub) where

import Prelude (($),undefined)

newtype Number = Number { fold :: forall a. (a -> a) -> a -> a }

zero :: Number
zero = Number $ \ f x -> x

succ :: Number -> Number
succ n = Number $ \ f x -> f $ fold n f x

one :: Number
one = succ zero

pred :: Number -> Number
pred n = fold n (const )

sub :: Number -> Number -> Number
sub = undefined
