module ChurchBooleans (n,o,t,a,d,r,x) where

type Boolean = forall a. a -> a -> a -- this requires RankNTypes

false,true :: Boolean
false = \ t f -> f
true  = \ t f -> t

-- true  n o t `shouldBe` false
-- true  a n d  false `shouldBe` false
-- false  o r  true `shouldBe` true

n = id
o = const true
t = false
a = flip const
d = const false
r = id
x = undefined
