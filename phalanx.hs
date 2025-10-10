{-# LANGUAGE LambdaCase #-}

module Phalanx (Name,Phalanx,rep,l,interp) where

import Data.String (IsString(..))

type Name = String

data Phalanx
  = Literal String
  | Empty
  | Join Phalanx Phalanx
  | Repeat Int Phalanx
  | Lookup Name

instance IsString Phalanx where
  fromString = Literal

instance Semigroup Phalanx where
  (<>) = Join

instance Monoid Phalanx where
  mempty = Empty

rep :: Int -> Phalanx -> Phalanx
rep = Repeat

l :: Name -> Phalanx
l = Lookup

interp :: (Name -> Maybe String) -> (Phalanx -> Maybe String)
interp lookup = \case
  Literal str  -> Just str
  Lookup name  -> lookup name
  Join p1 p2   -> do
    str1 <- interp lookup p1
    str2 <- interp lookup p2
    return $ str1 ++ str2
  Repeat n p   -> do
    str <- interp lookup p
    return $ concat $ replicate n str

