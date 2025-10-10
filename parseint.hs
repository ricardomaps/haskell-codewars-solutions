module ParseInt where
import Control.Monad  (join)
import Data.Bifunctor (bimap, second)

parseInt :: String -> Int
parseInt = go 1 . reverse . words
  where
    go :: Int -> [String] -> Int
    go _   []       = 0
    go pow (w : ws) =
      case w of
        "hundred"   -> go (100 * pow) ws
        "thousand"  -> go 1000 ws
        "million"   -> go 1_000_000 ws
        "and"       -> go pow ws
        _           -> vals w * pow + go pow ws

    vals v =
      case v of
        "zero"      -> 0
        "one"       -> 1
        "two"       -> 2
        "three"     -> 3
        "four"      -> 4
        "five"      -> 5
        "six"       -> 6
        "seven"     -> 7
        "eight"     -> 8
        "nine"      -> 9
        "ten"       -> 10
        "eleven"    -> 11
        "twelve"    -> 12
        "thirteen"  -> 13
        "fourteen"  -> 14
        "fifteen"   -> 15
        "sixteen"   -> 16
        "seventeen" -> 17
        "eighteen"  -> 18
        "nineteen"  -> 19
        "twenty"    -> 20
        "thirty"    -> 30
        "forty"     -> 40
        "fifty"     -> 50
        "sixty"     -> 60
        "seventy"   -> 70
        "eighty"    -> 80
        "ninety"    -> 90
        -- the input is always valid so we can rely on this being correct
        _           -> uncurry (+) . join bimap vals . second tail . break (== '-') $ v

