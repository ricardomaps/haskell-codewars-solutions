{-
  We need BlockArguments for a bit nicer syntax in our imperative code.

  With BlockArguments:              Without BlockArguments:
  while a (<) 3 do                  while a (<) 3 $ do
    ...                               ...

  We need RebindableSyntax because we rewrite (>>=), (>>) and return,
  but still want to use do-notation.
-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoPolyKinds #-} -- you might not need this once you have defined all types, it prevents inference errors in the initial solution

module Imperative where

import Data.STRef (STRef, readSTRef, newSTRef, modifySTRef)
import Control.Monad.ST
import Prelude hiding (return, (>>), (>>=))
import qualified Prelude as P

-- to implement mutable state we will use the ST monad
-- our variables will be STRefs accordingly
type Var = STRef

-- Imperative will be our main type that our imperative operations run it.
-- It takes a state of type `before` and returns a state of type `after`.
-- This will get used once we get to if/elif/else
-- The `st` type will be needed by the ST monad.

-- you will need to complete this type definition
newtype Imperative st before after a = Imp { runImperative :: ST st a }

-- Compare this to the type definition of Preludes (>>=).
-- Why can't we make `Imperative` a monad and use Preludes (>>=)?
(>>=) :: Imperative st s t a -> (a -> Imperative st t u b) -> Imperative st s u b
f >>= g = Imp $ runImperative f P.>>= runImperative . g

-- We could generalize (>>=)/(>>)/return to work on anything that
-- is known as an 'indexed monad' (we don't do that here though).
(>>) :: Imperative st p q a -> Imperative st q r b -> Imperative st p r b
f >> g = Imp $ runImperative f P.>> runImperative g

return :: a -> Imperative st s s a
return a = Imp (P.return a)

def :: (forall st. Imperative st NothingYet s (Var st a)) -> a
def s = runST $ runImperative $ s >>= getValue

var :: a -> Imperative st s s (Var st a)
var a = Imp (newSTRef a)

-- Now it's time to make use of our state.
-- We use these two types to prohibit invalid uses of elif/else.
-- Invalid examples: two consecutive elses, elif without an if before it
data NothingYet = NothingYet

data SawIf = SawIfRun | SawIfNotRun

{-
  Next, let's introduce a new typeclass so that we can use
  variables and values interchangeably in certain places.

  a <- var 1
  a += 1 -- add a variable and a value
  a += a -- add a variable and a variable

  b <- var 2
  if a (<) b do -- compare variable with a variable
    ...
  if a (<) 2 do -- compare a variable with a value
    ...
  while 1 (<) 2 do -- compare a value with a value
    ...
-}
class HasValue st a b where
  getValue :: forall s. a -> Imperative st s s b

instance {-# OVERLAPPING #-} (a ~ b) => HasValue st (Var st a) b where
  getValue a = Imp (readSTRef a)

instance (a ~ b) => HasValue st a b where
  getValue a = Imp (P.return a)

while :: (HasValue st v a, HasValue st w b) => v -> (a -> b -> Bool) -> w -> Imperative st s t r -> Imperative st s s ()
while v pred w r = undefined

if' :: (HasValue st v a, HasValue st w b) => v -> (a -> b -> Bool) -> w -> Imperative st s s r -> Imperative st s SawIf ()
if' = error "implement if"

elif' :: (HasValue st v a, HasValue st w b) => v -> (a -> b -> Bool) -> w -> Imperative st SawIf s r -> Imperative st SawIf SawIf ()
elif' = error "implement elif"

else' :: Imperative st SawIf s b -> Imperative st SawIf NothingYet ()
else' = error "implement else"

(+=) :: (Num a, HasValue st w a) => Var st a -> w -> Imperative st s s ()
v += w = modifyVar v w (+)

(-=) :: (Num a, HasValue st w a) => Var st a -> w -> Imperative st s s ()
v -= w = modifyVar v w (-)

(*=) :: (Num a, HasValue st w a) => Var st a -> w -> Imperative st s s ()
v *= w = modifyVar v w (*)

(.=) :: (Num a, HasValue st w a) => Var st a -> w -> Imperative st s s ()
v .= w = modifyVar v w  (flip const)

push :: (HasValue st w a) => Var st [a] -> w -> Imperative st s s ()
push v w = modifyVar v w (\l x -> l ++ [x])

toString :: (Show a) => Var st a -> Imperative st s s String
toString v = getValue v >>= (\a -> Imp (P.return $ show a))

modifyVar :: (HasValue st w b) => Var st a -> w -> (a -> b -> a) -> Imperative st s s ()
modifyVar v w f = do
  a <- getValue w
  Imp (modifySTRef v (flip f a))
