{-# LANGUAGE ScopedTypeVariables, Rank2Types #-}
module ScottEncoding where

import Prelude hiding (null, length, map, filter, head, tail, foldl, foldr, take, fst, snd, curry, uncurry, concat, zip, (++))

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toPair :: SPair a b -> (a,b)
toPair p = runPair p (,)
fromPair :: (a,b) -> SPair a b
fromPair (x, y) = SPair (\f -> f x y)
fst :: SPair a b -> a
fst p = runPair p const
snd :: SPair a b -> b
snd p = runPair p (flip const)
swap :: SPair a b -> SPair b a
swap p = fromPair (snd p, fst p)
curry :: (SPair a b -> c) -> (a -> b -> c)
curry f = \x y -> f (fromPair (x, y))
uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f = \p -> runPair p f

toMaybe :: SMaybe a -> Maybe a
toMaybe m = runMaybe m Nothing Just
fromMaybe :: Maybe a -> SMaybe a
fromMaybe m = SMaybe (\b f -> case m of Nothing -> b; Just a -> f a)
isJust :: SMaybe a -> Bool
isJust m = runMaybe m False (const True)
isNothing :: SMaybe a -> Bool
isNothing m = runMaybe m True (const False)
catMaybes :: SList (SMaybe a) -> SList a
catMaybes = map (\x -> runMaybe x undefined id) . filter isJust

toEither :: SEither a b -> Either a b
toEither e = runEither e Left Right
fromEither :: Either a b -> SEither a b
fromEither e = SEither (\f g -> case e of Left a -> f a; Right b -> g b)
isLeft :: SEither a b -> Bool
isLeft e = runEither e (const True) (const False)
isRight :: SEither a b -> Bool
isRight e = runEither e (const False) (const True)
partition :: SList (SEither a b) -> SPair (SList a) (SList b)
unLeft :: SEither a b -> a
unLeft e = runEither e id undefined
unRight :: SEither a b -> b
unRight e = runEither e undefined id
partition l = fromPair (map unLeft (filter isLeft l), map unRight (filter isRight l))

toList :: SList a -> [a]
toList = foldr (:) []
fromList :: [a] -> SList a
fromList l = SList (\b f -> case l of [] -> b; (x:xs) -> f x (fromList xs))
cons :: a -> SList a -> SList a
cons x l = SList (\_ f -> f x l)
head :: SList a -> a
head l = runList l undefined const
tail :: SList a -> SList a
tail l = runList l undefined (flip const)
concat :: SList a -> SList a -> SList a
concat xs ys = foldr cons ys xs
nil :: SList a
nil = SList (\b _ -> b)
null :: SList a -> Bool
null l = runList l True (const (const False))
length :: SList a -> Int
length = foldr (\_ b -> b + 1) 0
map :: (a -> b) -> SList a -> SList b
map f = foldr (\x xs -> cons (f x) xs) nil
filter :: (a -> Bool) -> SList a -> SList a
filter f l = runList l nil (\x xs -> if f x then cons x (filter f xs) else filter f xs)
zip :: SList a -> SList b -> SList (SPair a b)
zip xs ys
  | null xs || null ys = nil
  | otherwise = cons (fromPair (head xs, head ys)) (zip (tail xs) (tail ys))
foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f a l = foldr (\x g a -> g (f a x)) id l a
foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f a l = runList l a (\x xs -> f x (foldr f a xs))
take :: Int -> SList a -> SList a
take n l = if n == 0 then nil else runList l nil (\x xs -> cons x (take (n-1) xs))
