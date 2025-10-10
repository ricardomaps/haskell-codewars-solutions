module ListsAsFolds (cons,nil,sum,product,iterate,repeat,cycle,replicate,null,length,snoc,append,concat,map,filter,take,drop,splitAt,get,set,any,all,find,findIndex,partition,span,minimumBy,maximumBy,sortBy,foldl,scanl,scanr,reverse,head,tail,init,last,zip,unzip,zipWith) where

import Prelude ()
import Preloaded

-- primitive operations

cons :: a -> List a -> List a
cons = undefined

nil :: List a
nil = undefined

-- derived operations

sum,product :: List Number -> Number
sum = undefinedHelper "sum"
product = undefinedHelper "product"

-- derived constructors

iterate :: (a -> a) -> a -> List a
iterate = undefined

repeat :: a -> List a
repeat = undefined

cycle :: List a -> List a
cycle = undefined

replicate :: Number -> a -> List a
replicate = undefined

-- more derived operations

null :: List a -> Boolean
null = undefined

length :: List a -> Number
length = undefined

snoc :: List a -> a -> List a
snoc = undefined

append :: List a -> List a -> List a
append = undefined

concat :: List (List a) -> List a
concat = undefined

map :: (a -> z) -> List a -> List z
map = undefined

filter :: (a -> Boolean) -> List a -> List a
filter = undefined

take :: Number -> List a -> List a
take = undefinedHelper "take"

drop :: Number -> List a -> List a
drop = undefined

splitAt :: Number -> List a -> Pair (List a) (List a)
splitAt = undefined

get :: Number -> List a -> Option a
get = undefined

set :: Number -> a -> List a -> List a
set = undefined

any :: (a -> Boolean) -> List a -> Boolean
any = undefined

all :: (a -> Boolean) -> List a -> Boolean
all = undefined

find :: (a -> Boolean) -> List a -> Option a
find = undefined

findIndex :: (a -> Boolean) -> List a -> Option Number
findIndex = undefined

partition :: (a -> Boolean) -> List a -> Pair (List a) (List a)
partition = undefined

span :: (a -> Boolean) -> List a -> Pair (List a) (List a)
span = undefined

minimumBy :: (a -> a -> Boolean) -> List a -> Option a
minimumBy = undefined

maximumBy :: (a -> a -> Boolean) -> List a -> Option a
maximumBy = undefined

sortBy :: (a -> a -> Boolean) -> List a -> List a
sortBy = undefined

foldl :: List a -> (z -> a -> z) -> z -> z
foldl = undefined

scanl :: List a -> (z -> a -> z) -> z -> List z
scanl = undefined

scanr :: List a -> (a -> z -> z) -> z -> List z
scanr = undefined

reverse :: List a -> List a
reverse = undefined

head :: List a -> Option a
head = undefined

tail :: List a -> Option (List a)
tail = undefined

init :: List a -> Option (List a)
init = undefined

last :: List a -> Option a
last = undefined

zip :: List a -> List b -> List (Pair a b)
zip = undefined

unzip :: List (Pair a b) -> Pair (List a) (List b)
unzip = undefined

zipWith :: (a -> b -> z) -> List a -> List b -> List z
zipWith = undefined
