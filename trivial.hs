module TrivialTauto where
-- define p1, p2, p3 below

p1 :: (a -> b, b -> c) -> (a -> c)
p1 (f, g) = g . f

p2 :: (a -> b -> c) -> (a, b) -> c
p2 f (x, y) = f x y

p3 :: ((Either a b, a -> c), b -> c) -> c
p3 ((ab, f), g) =
  case ab of
    Right b -> g b
    Left  a -> f a

-- 1) ((A ⇒ B) ∧ (B ⇒ C)) ⇒ (A ⇒ C)
-- 2) (A ⇒ B ⇒ C) ⇒ ((A ∧ B) ⇒ C)
-- 3) ((A ∨ B) ∧ (A ⇒ C) ∧ (B ⇒ C)) ⇒ C
