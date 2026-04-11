module Automaton (readCommands) where 

data State = Q1 | Q2 | Q3

readCommands :: [Char] -> Bool
readCommands commands =
  case foldl step Q1 commands of
    Q2 -> True
    _  -> False
  where
  step Q1 '1' = Q2
  step Q2 '0' = Q3
  step Q3  _  = Q2
  step st  _  = st
