module Kata.Sentencify (sentencify) where
import Data.Char (toUpper)

sentencify :: [String] -> String
sentencify = (++ ".") . capitalize . unwords
  where capitalize (h:t) = toUpper h : t

