module MysteryFunction (mystery,mysteryInv,nameOfMystery) where
import Data.Bits

mystery :: Int -> Int
mystery = xor <*> flip shiftR 1

mysteryInv :: Int -> Int
mysteryInv num = go num
  where
    go acc = foldr (\i n -> if testBit n (i+1) then complementBit n i else n) num [0..findHighest num 0]
    findHighest n i = if n > 0 then findHighest (shiftR n 1) (i+1) else (i-1)

nameOfMystery :: String
nameOfMystery = "undefined"
