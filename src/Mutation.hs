{-# LANGUAGE TypeFamilies #-}

module Mutation where

import Control.Monad.ST (ST)
import Data.IORef       (newIORef, readIORef, writeIORef, IORef)
import Data.STRef       (newSTRef, readSTRef, writeSTRef, STRef)

class Mutation m where
    type Ref m :: * -> *
    newRef :: a -> m (Ref m a)
    readRef :: Ref m a -> m a
    writeRef :: Ref m a -> a -> m ()

instance Mutation IO where
    type Ref IO = IORef
    newRef = newIORef
    readRef = readIORef
    writeRef = writeIORef

data T m a = MkT [Ref m a]


instance Mutation (ST s) where
    type Ref (ST s) = STRef s
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef


writeSomeStuff :: Mutation m => m (Ref m Int)
writeSomeStuff = newRef 5

writeSomeStuffIO :: IO ()
writeSomeStuffIO = do
    ref <- writeSomeStuff
    num <- readRef ref
    print num

readAndPrint :: IO ()
readAndPrint = do
    r <- newRef 'x'
    v <- readRef r
    print v
