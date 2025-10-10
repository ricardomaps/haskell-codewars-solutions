module Wrap where
import Data.Proxy
import Data.Typeable

data Scheme a where
    Res :: Typeable a => Proxy a -> Scheme a
    Arg :: Typeable a => Proxy a -> Scheme b -> Scheme (a -> b)

data Function = forall a. Function (Scheme a) a

newtype Wrap a = Wrap
    { unWrap :: a
    }

twoArgs :: (Typeable a, Typeable b, Typeable c) => Scheme (a -> b -> c)
twoArgs = Arg Proxy . Arg Proxy $ Res Proxy

plusFunction :: Function
plusFunction = Function twoArgs plusInt where
    plusInt :: Int -> Int -> Int
    plusInt = (+)

wrapFunction :: Function -> Function
wrapFunction (Function (Res _) x)   = Function (Res Proxy) (Wrap x)
wrapFunction (Function (Arg _ s) f) = undefined
