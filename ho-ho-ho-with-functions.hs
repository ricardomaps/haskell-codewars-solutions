module Ho (ho) where

class Ho a where
  ho :: a -> String
  
instance Ho () where
  ho () = "Ho!"
  
instance Ho String where
  ho s = "Ho " ++ s
