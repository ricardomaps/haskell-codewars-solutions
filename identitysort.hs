{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module IdentitySort where

import           Data.Kind

data Natural :: Type -> Type where
  NumZ :: Natural '[]
  NumS :: Natural n -> Natural (() : n)

data SortedList :: [Type] -> Type where
  -- Cons :: 
  -- Nil :: SortedList '[]

identitySort :: SortedList n -> SortedList n
identitySort x = x
