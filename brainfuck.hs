module Brainfuck (brainfuck) where
import qualified Data.Array as Array
import Data.Char (chr, ord)

-- Inspired from real-world Brainf**k, we want to create an interpreter of that language which will support the following instructions:

--     > increment the data pointer (to point to the next cell to the right).
--     < decrement the data pointer (to point to the next cell to the left).
--     + increment (increase by one, truncate overflow: 255 + 1 = 0) the byte at the data pointer.
--     - decrement (decrease by one, treat as unsigned byte: 0 - 1 = 255 ) the byte at the data pointer.
--     . output the byte at the data pointer.
--     , accept one byte of input, storing its value in the byte at the data pointer.
--     [ if the byte at the data pointer is zero, then instead of moving the instruction pointer forward to the next command, jump it forward to the command after the matching ] command.
--     ] if the byte at the data pointer is nonzero, then instead of moving the instruction pointer forward to the next command, jump it back to the command after the matching [ command.

-- The function will take in input...

--     the program code, a string with the sequence of machine instructions,
--     the program input, a string, possibly empty, that will be interpreted as an array of bytes using each character's ASCII code and will be consumed by the , instruction

-- ... and will return ...

--     the output of the interpreted code (always as a string), produced by the . instruction.

-- -- Implementation-specific details for this Kata:

-- --     Your memory tape should be large enough - the original implementation had 30,000 cells but a few thousand should suffice for this Kata
-- --     Each cell should hold an unsigned byte with wrapping behavior (i.e. 255 + 1 = 0, 0 - 1 = 255), initialized to 0
-- --     The memory pointer should initially point to a cell in the tape with a sufficient number (e.g. a few thousand or more) of cells to its right. For convenience, you may want to have it point to the leftmost cell initially
-- --     You may assume that the , command will never be invoked when the input stream is exhausted
-- --     Error-handling, e.g. unmatched square brackets and/or memory pointer going past the leftmost cell is not required in this Kata. If you see test cases that require you to perform error-handling then please open an Issue in the Discourse for this Kata (don't forget to state which programming language you are attempting this Kata in).


brainfuck :: String -> String -> String
brainfuck code input = map chr . reverse $ go tape [] input 0 0
  where
    tape = Array.listArray (0, 4999) $ replicate 5000 (0 :: Int)
    instructions = Array.listArray (0, length code-1) code
    end = snd . Array.bounds $ instructions
    findNext i nest =
      case instructions Array.! i of
        '[' -> findNext (i+1) (nest+1)
        ']' -> if nest == 0 then i+1 else findNext (i+1) (nest-1)
        _   -> findNext (i+1) nest
    findPrevious i nest = 
      case instructions Array.! i of
        ']' -> findPrevious (i-1) (nest+1)
        '[' -> if nest == 0 then i+1 else findPrevious (i-1) (nest-1)
        _   -> findPrevious (i-1) nest
    go tape output input ptr i =
      if i > end then output else
      case instructions Array.! i of
        '<' -> go tape output input (ptr-1) (i+1)
        '>' -> go tape output input (ptr+1) (i+1)
        '.' -> go tape (tape Array.! ptr:output) input ptr (i+1)
        ',' -> go (tape Array.// [(ptr, ord $ head input)]) output (tail input) ptr (i+1)
        '+' -> go (tape Array.// [(ptr, (tape Array.! ptr + 1) `mod` 256)]) output input ptr (i+1)
        '-' -> go (tape Array.// [(ptr, (tape Array.! ptr - 1) `mod` 256)]) output input ptr (i+1)
        '[' ->
          if tape Array.! ptr == 0 then
            go tape output input ptr (findNext (i+1) 0)
          else
            go tape output input ptr (i+1)
        ']' ->
          if tape Array.! ptr /= 0 then
            go tape output input ptr (findPrevious (i-1) 0)
          else
            go tape output input ptr (i+1)

        
