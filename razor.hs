module Razor where

data Razor
  = Lit Int
  | Add Razor Razor
  
interpret :: Razor -> Int
interpret (Lit i)     = i
interpret (Add x1 x2) = interpret x1 + interpret x2

pretty :: Razor -> String
pretty (Lit i)     = show i
pretty (Add x1 x2) = "(" <> pretty x1 <> "+" <> pretty x2 <> ")"
