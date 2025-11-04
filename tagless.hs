{-# LANGUAGE RankNTypes #-}

module Tagless where

import Prelude hiding (and, or)
import Data.Function (fix)

class Language r where
  here   :: r (a, h) a
  before :: r h a -> r (any, h) a
  lambda :: r (a, h) b -> r h (a -> b)
  apply  :: r h (a -> b) -> (r h a -> r h b)
  
  loop   :: r h (a -> a) -> r h a
  
  int    :: Int -> r h Int
  add    :: r h Int -> r h Int -> r h Int
  down   :: r h Int -> r h Int    -- \x -> x - 1
  up     :: r h Int -> r h Int    -- \x -> x + 1
  mult   :: r h Int -> r h Int -> r h Int
  gte    :: r h Int -> r h Int -> r h Bool -- greater than or equal
  
  bool   :: Bool -> r h Bool
  and    :: r h Bool -> r h Bool -> r h Bool
  or     :: r h Bool -> r h Bool -> r h Bool
  neg    :: r h Bool -> r h Bool
  
  ifte   :: r h Bool -> r h a -> r h a -> r h a -- if true then return left term, else return right term

type Term a = forall r h . Language r => r h a

newtype Lang h a = Lang { run :: h -> a }

instance Language Lang where
  here     = Lang fst
  before r = Lang (\(_, h') -> let a = run r h' in a)
  lambda r = Lang (\h a -> run r (a, h))
  apply rf r = Lang (\h -> let f = run rf h; a = run r h in f a)
  loop rf = Lang (\h -> let f = run rf h in fix f)
  int n = Lang (const n)
  bool b = Lang (const b)
  add r1 r2 = Lang (\h -> let x1 = run r1 h; x2 = run r2 h in x1 + x2)
  down r = Lang (\h -> let x = run r h in x-1)
  up r = Lang (\h -> let x = run r h in x+1)
  mult r1 r2 = Lang (\h -> let x1 = run r1 h; x2 = run r2 h in x1 * x2)
  gte r1 r2 = Lang (\h -> let x1 = run r1 h; x2 = run r2 h in x1 >= x2)
  and r1 r2 = Lang (\h -> let x1 = run r1 h; x2 = run r2 h in x1 && x2)
  or r1 r2 = Lang (\h -> let x1 = run r1 h; x2 = run r2 h in x1 || x2)
  neg r = Lang (\h -> let b = run r h in not b)
  ifte rb rt re = Lang (\h -> let b = run rb h in if b then run rt h else run re h)

-- ex :: Term (Int -> Int -> Int)
-- ex = lambda (lambda (add (before here) here))

interpret :: Term a -> a
interpret t = run t ()
