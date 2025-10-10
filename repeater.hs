module Repeater (makeLooper) where
import Data.IORef

makeLooper :: String -> IO (IO Char)
makeLooper str = do
  stream' <- newIORef (cycle str)
  return $ do
    stream <- readIORef stream'
    modifyIORef stream' tail
    return $ head stream
