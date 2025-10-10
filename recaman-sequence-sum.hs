module RecamanSequenceSum (rec) where
import qualified Data.IntSet as IS 

recaman :: [Int]
recaman = 0 : go (IS.singleton 0) 1 0
  where 
  go seen idx prev = 
    let n = if idx > prev || IS.member (prev-idx) seen then prev+idx else prev-idx
    in n : go (IS.insert n seen) (succ idx) n
    
rec :: Int -> Int
rec = sum . (flip take recaman)
