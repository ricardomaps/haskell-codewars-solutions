module ContiguousVowels (sortByVowels) where
import Data.List (sortOn)
import Data.Ord (Down(..))

sortByVowels :: [String] -> [String]
sortByVowels = sortOn (Down . maxContiguousVowels)
  where
  vowels = "aeiouAEIOU"
  isVowel = (`elem` vowels)
  isConsonant = not . isVowel
  vowelGroups "" = [""]
  vowelGroups xs =
    let rest       = dropWhile isConsonant xs
        (g, rest') = span isVowel rest
    in  g : vowelGroups rest'
  maxContiguousVowels = maximum . map length . vowelGroups
