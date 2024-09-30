module ContinuedFraction (continuedFraction) where

continuedFraction :: Integer -> Integer -> [Integer]
continuedFraction _ 0 = []
continuedFraction 0 _ = []
continuedFraction numerator 1 = [numerator]
continuedFraction numerator denominator = 
  numerator `div` denominator : continuedFraction denominator (numerator `rem` denominator)
