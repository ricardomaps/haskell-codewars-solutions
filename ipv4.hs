module IPv4 where
import Data.Word  (Word32)
import Data.Bits (shiftR, (.&.))
import Data.List (intercalate)

type IPString = String

word32ToIP :: Word32 -> IPString
word32ToIP = intercalate "." . reverse . map show . map (.&. 255) . map toInteger . take 4 . iterate ((flip shiftR) 8)
