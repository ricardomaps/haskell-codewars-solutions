module Recaman (recaman) where
import qualified Data.Set as S

recamanSeq :: [Int]
recamanSeq = 0 : genRecaman S.empty 1 0
  where 
  genRecaman seen idx prev = 
    let n = if prev-idx <= 0 || S.member (prev-idx) seen then prev+idx else prev-idx
    in n : genRecaman (S.insert n seen) (succ idx) n

recaman :: Int -> Int
recaman n = recamanSeq !! n
