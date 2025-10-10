{-# LANGUAGE TemplateHaskell #-}
module Kata.TupleMaker (tuple) where

import Language.Haskell.TH

-- | Creates a lambda that takes `n` arguments and
-- | returns an n-tuple of those arguments.
tuple :: Int -> Q Exp
tuple 1 = [| \x -> x |]
tuple n = do
  let vars = map (\i -> mkName $ "x" ++ show i) [1..n]
  return $ LamE (VarP <$> vars) (TupE (Just . VarE <$> vars))
