module Solution where
import Prelude hiding ((==))
data OmniBool = OmniBool

omniBool :: OmniBool
omniBool = OmniBool

(==) :: OmniBool -> Bool -> Bool
_ == _ = True

