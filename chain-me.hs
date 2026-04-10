module ChainMe (chain) where
import Data.Function ((&))

chain :: x -> [ x -> x ] -> x
chain = foldl (&)
