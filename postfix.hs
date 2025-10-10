module Postfix where

begin c              = c []
push stack n c       = c (n:stack)
add  (x:y:stack) c   = c (x+y:stack)
end  (ret:_)         = ret
