{-# LANGUAGE FunctionalDependencies #-}

module Collects where

class Collects e ce | ce -> e where

  empty :: ce

  insert :: e -> ce -> ce

  member :: e -> ce -> Bool

f x y coll = insert x (insert y coll)
