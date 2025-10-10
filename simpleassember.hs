module SimpleAssembler (simpleAssembler) where
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Text.Read
import Data.Array

type Registers = M.Map String Int

simpleAssembler :: [String] -> Registers
simpleAssembler ins = go (M.empty) 1
  where
  inst  = listArray (1, length ins) ins
  getVal x m =
    case readMaybe x of
      Just y  -> y
      Nothing -> fromJust (M.lookup x m)
  go reg pc
    | pc > snd (bounds inst) = reg
    | otherwise =
        case words (inst ! pc) of
          ["mov", x, y] -> go (M.insert x (getVal y reg) reg) (pc+1)
          ["inc", x]    -> go (M.adjust succ x reg) (pc+1)
          ["dec", x]    -> go (M.adjust pred x reg) (pc+1)
          ["jnz", x, y] -> if getVal x reg /= 0 then go reg (pc + getVal y reg) else go reg (pc+1)
          _             -> error "invalid opcode"

