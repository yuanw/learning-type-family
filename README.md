# Just Enough Type-level programming in GHC

One of the difficulties in learning haskell is to be productive with Haskell, you need to know many aspects of the language. For example, how to use servant or beam, you need to know a little about lenses and type-level programming in haskell

## Goals

to able to use libaraies such Beam and Servant

## Path
* [MultiParamTypeClasses](#multiparamtypeclasses)
* Functional Depdencies
* Type Families
* GADTs
* Data Type/Kind Promotion
* Kind Polymorphism

## Moivation

Why do we care about Type-level programming ?

TODO

## MultiParamTypeClasses

https://wiki.haskell.org/Multi-parameter_type_class

By default, Haskell [`type classes`](https://en.wikipedia.org/wiki/Type_class) can only have an argument. To have mutiple arguments, use the `{-# LANGUAGE MultiParamTypeClasses #-}`.

For example, the following code won't compile
```haskell
module Mutation where

class Mutation m r where
    newRef :: a -> m (r a)
    readRead :: r a -> m a
    writeRef :: r a -> a -> m ()
```
```bash
   • Too many parameters for class ‘Mutation’
      (Enable MultiParamTypeClasses to allow multi-parameter classes)
    • In the class declaration for ‘Mutation’
  |
3 | class Mutation m w where
  | ^^^^^^^^^^^^^^^^^^^^^^^^...
```

[Type classes: an exploration of the design space](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=2ahUKEwiqk4q7nIHlAhURZd8KHd8YDz4QFjAAegQIARAC&url=https%3A%2F%2Fwww.microsoft.com%2Fen-us%2Fresearch%2Fwp-content%2Fuploads%2F1997%2F01%2Fmulti.pdf&usg=AOvVaw3ql-6z48-7kUDS4SOrnsuV) points out 3 kinds of cases when multi-parameter type classes are needed:

### Overloading with coupled parameters
1. Mutation case
2. State Monad
```haskell
class Monad m => StateMonad m s where
    getS :: m s
    putS :: s -> m()
```
3. Reader Monad
```haskell
class Monad m => Reader m e where
  env :: e -> m a -> m a
  getEnv :: m e
```
### Overloading with constrained parameters

[Type Classes vs. the world](https://www.youtube.com/watch?v=hIZxTQP1ifo)


If you think of a single-parameter type class as a set of types, then a multi-parameter type class is a relation between types.

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}

module Mutation where

import Control.Monad.ST (ST)
import Data.IORef       (newIORef, readIORef, writeIORef, IORef)
import Data.STRef       (newSTRef, readSTRef, writeSTRef, STRef)

class Mutation m r where
    newRef :: a -> m (r a)
    readRef :: r a -> m a
    writeRef :: r a -> a -> m ()

instance Mutation IO IORef where
    newRef = newIORef
    readRef = readIORef
    writeRef = writeIORef

instance Mutation (ST s) (STRef s) where
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef


writeSomeStuff :: Mutation m r => m (r Int)
writeSomeStuff = newRef 5

writeSomeStuffIO :: IO ()
writeSomeStuffIO = do
    ref <- writeSomeStuff
    num <- readIORef ref
    print num

readAndPrint :: IO ()
readAndPrint = do
    r <- newRef 'x' :: IO (IORef Char)
    v <- readRef r
    print v

```

## Functional Dependencies

Type Classes with Functional Depdendencies:
>> Type classes in Haskell allow programmers to define functions that can be used on a set of different types, with a potentially different implementation in the each caes.
(aka overloading)

>> One of the problems with multi-parameter type class is the relation on types that we can specify is too general. It fails to capture important dependenice bewteen parameters. More concretely, the use of mutiple parameter classes can often result in ambiguities and inaccuracies in inferred types.

```haskell
{-# LANGUAGE FunctionalDependencies #-}

module Collects where

class Collects e ce | ce -> e where
    empty :: ce
    insert :: e -> ce -> ce
    member :: e -> ce -> Bool
```
by annotating the class defintion with a depency `ce -> e`, to be read as "ce uniquely determines e."

Another example `FiniteMaps`

```haskell
class FiniteMap i e fm | fm -> (i, e) where
  emptyFM :: fm
  lookup :: i -> fm -> Maybe e
  extend :: i -> e -> fm -> fm
```

[https://wiki.haskell.org/Functional_dependencies](https://wiki.haskell.org/Functional_dependencies)

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#functional-dependencies

So far, we haven't talked about type leve programming.

TODO decide whether needed to talk about [Fun with Functional Dependencies](http://www.cse.chalmers.se/~hallgren/Papers/hallgren.pdf)

```haskell
{-# LANGUAGE FunctionalDependencies #-}

module Mutation where

import Control.Monad.ST (ST)
import Data.IORef       (newIORef, readIORef, writeIORef, IORef)
import Data.STRef       (newSTRef, readSTRef, writeSTRef, STRef)

class Mutation m r | m -> r where
    newRef :: a -> m (r a)
    readRef :: r a -> m a
    writeRef :: r a -> a -> m ()

instance Mutation IO IORef where
    newRef = newIORef
    readRef = readIORef
    writeRef = writeIORef

instance Mutation IO (STRef s) where
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef

instance Mutation (ST s) (STRef s) where
    newRef = newSTRef
    readRef = readSTRef
    writeRef = writeSTRef
```

won't compile

```bash
/Users/yuanwang/workspaces/learning-type-family/src/Mutation.hs:14:10: error:
    Functional dependencies conflict between instance declarations:
      instance Mutation IO IORef -- Defined at src/Mutation.hs:14:10
      instance Mutation IO (STRef s) -- Defined at src/Mutation.hs:19:10
   |
14 | instance Mutation IO IORef where
   |          ^^^^^^^^^^^^^^^^^

/Users/yuanwang/workspaces/learning-type-family/src/Mutation.hs:19:10: error:
    • Illegal instance declaration for ‘Mutation IO (STRef s)’
        The coverage condition fails in class ‘Mutation’
          for functional dependency: ‘m -> r’
        Reason: lhs type ‘IO’ does not determine rhs type ‘STRef s’
        Un-determined variable: s
    • In the instance declaration for ‘Mutation IO (STRef s)’
   |
19 | instance Mutation IO (STRef s) where
   |          ^^
```

## Type Families

```haskell
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
```

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Add where

class Add a b where
    type SumTy a b
    add :: a -> b -> SumTy a b

instance Add Integer Double where
    type SumTy Integer Double = Double
    add x y = fromIntegral x + y

instance Add Integer Float where
    type SumTy Integer Float = Float
    add x y = fromIntegral x + y


x = 10 :: Float
y = 3 :: Integer

z = add y x
```

## GADT

https://www.youtube.com/watch?v=6snteFntvjM
