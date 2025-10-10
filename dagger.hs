{-# LANGUAGE LambdaCase #-}

module Dagger where

data Dagger
  = Literal  Double
  | Add      Dagger Dagger
  | Multiply Dagger Dagger
  | Absolute Dagger
  | Signum   Dagger
  | Negate   Dagger
  | Divide   Dagger Dagger

instance Num Dagger where
  (+) = Add
  signum = Signum
  abs = Absolute
  (*) = Multiply
  negate = Negate
  fromInteger = Literal . fromInteger

instance Fractional Dagger where
  (/) = Divide
  fromRational = Literal . fromRational

interp :: Dagger -> Maybe Double
interp = \case
  Literal n      -> Just n
  Add d1 d2      -> (+) <$> interp d1 <*> interp d2
  Multiply d1 d2 -> (*) <$> interp d1 <*> interp d2
  Absolute d     -> abs <$> interp d
  Signum d       -> signum <$> interp d
  Negate d       -> negate <$> interp d
  Divide d1 d2   -> do
    numerator   <- interp d1
    denominator <- interp d2
    if denominator == 0 then
      Nothing
    else
      return $ numerator / denominator

