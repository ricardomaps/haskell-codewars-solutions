module Coroutine where

import Control.Monad (ap, forever)
-- import Preloaded

-- Preloaded contains the following:

newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)

data Command r u d a =
    Done a
  | Out d (Coroutine r u d a)
  | In (u -> Coroutine r u d a)
  deriving Functor

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Applicative (Coroutine r u d) where
  pure = return
  (<*>) = ap

instance Monad (Coroutine r u d) where
  return x = Coroutine (\k -> k (Done x))
  f >>= g  = Coroutine
    (\k -> apply f
      (\cmd ->
         case cmd of
           Done a -> apply (g a) k
           Out d c -> k $ Out d (c >>= g)
           In f'   -> k $ In (\u -> f' u >>= g)))

(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
p1 >>> p2 = Coroutine $
  \k -> apply p2 $
    \cmd ->
      case cmd of
        Done a  -> k $ Done a
        Out d c -> k $ Out d (p1 >>> c)
        In f    -> apply p1 $
          \cmd' ->
            case cmd' of
              Done a  -> k $ Done a
              In f'   -> k $ In (\u -> f' u >>> p2)
              Out d c -> apply (c >>> f d) k

-- Library functions

output :: a -> Coroutine r u a ()
output v = Coroutine (\k -> k $ Out v (return ()))

input :: Coroutine r v d v
input = Coroutine (\k -> k $ In return)

produce :: [a] -> Coroutine r u a ()
produce []     = return ()
produce (x:xs) = do output x; (produce xs)

consume :: Coroutine [t] u t a -> [t]
consume c = apply c $
  \cmd ->
    case cmd of
      In _     -> error "can't provide input"
      Done _   -> []
      Out v c' -> v : consume c'

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = do
  u <- input
  if p u then do
    output u
    filterC p
  else
    filterC p

limit :: Int -> Coroutine r v v ()
limit n =
  if n <= 0 then
    return ()
  else do
    u <- input
    output u
    limit (n-1)

suppress :: Int -> Coroutine r v v ()
suppress n = do
  u <- input
  if n <= 0 then do
    output u
    suppress 0
  else
    suppress (n-1)

add :: Coroutine r Int Int ()
add = do
  u <- input
  v <- input
  output (u+v)
  add

duplicate :: Coroutine r v v ()
duplicate = do
  u <- input
  output u
  output u
  duplicate

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers 
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filterC even >>> limit 5
p2 = loop 1 0
  where
    loop i n = do
      output (i+n)
      loop (i+1) (i+n)
p3 = duplicate >>> add
p4 = duplicate >>> suppress 1 >>> add
