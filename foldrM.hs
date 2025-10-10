module FoldrM where
import Control.Monad (liftM, ap)

foldrM :: (Monad m) => (a -> b -> m b) -> b -> [a] -> m b
foldrM f b ta = let (l, r) = go l ta in r
  where
    go l []     = (return b, )
    go l (a:as) = let (l', r') = go l as in r' >>= f a

tIMEOUT :: Int
tIMEOUT = 10000000

data Nest a
    = Pure a
    | Nest (Nest a)

instance Functor Nest where
    fmap = liftM

instance Applicative Nest where
    pure = Pure
    (<*>) = ap

instance Monad Nest where
    m0 >>= f = go m0 where
        go (Pure x) = f x
        go (Nest m) = Nest $ go m

nestN :: Int -> a -> Nest a
nestN n0 x = go n0 where
    go 0 = Pure x
    go n = Nest . go $ n - 1

unNestN :: a -> Int -> Nest a -> a
unNestN z = go where
    go _ (Pure x) = x
    go 0 _        = z
    go n (Nest m) = go (n - 1) m

u :: a
u = error "this was not supposed to be forced"
