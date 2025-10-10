{-# Language RankNTypes #-}

module Church (not,and,or,xor) where

import Prelude hiding (Bool,False,True,not,and,or,(&&),(||),(==),(/=))

type Boolean = forall a. a -> a -> a -- this requires RankNTypes

false,true :: Boolean
false = \ t f -> f
true  = \ t f -> t

not :: Boolean -> Boolean
and,or,xor :: Boolean -> Boolean -> Boolean

not = \ a   -> a false true
and = \ a b -> a b false
or  = \ a b -> a true b
xor = \ a b -> and (not (and a b)) (or a b)
