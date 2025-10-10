module Haskell.Codewars.Peano where
import Prelude hiding (even, odd, div, compare, Num, Int, Integer, Float, Double, Rational, Word)

data Peano = Zero | Succ Peano deriving (Eq, Show)

add, sub, mul, div :: Peano -> Peano -> Peano
-- Addition
add Zero     m = m
add (Succ n) m = add n (Succ m)
-- Subtraction
sub Zero     (Succ _) = error "negative number"
sub n        Zero     = n
sub (Succ n) (Succ m) = sub n m
-- Multiplication
mul Zero     _ = Zero
mul (Succ n) m = add m (mul n m)
-- Integer division
div _ Zero = error "divide by 0"
div n m    = if compare n m /= LT then Succ (div (sub n m) m) else Zero

even, odd :: Peano -> Bool
-- Even
even Zero     = True
even (Succ n) = odd n

-- Odd
odd Zero     = False
odd (Succ n) = even n

compare :: Peano -> Peano -> Ordering
-- Compare
compare Zero     Zero     = EQ
compare Zero     (Succ _) = LT
compare (Succ _) Zero     = GT
compare (Succ n) (Succ m) = compare n m
